#!/usr/bin/env python3
# =============================================================================
# 09_arquivos_urna.py - Novos testes 1, 2, 4 e 5 com os ARQUIVOS DE URNA
#
#   N1. Recontagem RDV x BU: cada voto gravado no Registro Digital do Voto é
#       recontado e comparado com o Boletim de Urna da mesma seção, cargo a
#       cargo (esperado: 0 divergências). O R (script 10) compara ainda o BU
#       com o CSV oficial totalizado.
#   N2. Integridade e assinatura: SHA-512 do BU, do RDV e do log comparado
#       com o valor assinado pela urna no arquivo -vota.vsc, e verificação da
#       assinatura de hardware da urna (Ed521 ou ECDSA P-521) com o
#       certificado embutido. A cadeia até a AC raiz NÃO é verificada (os
#       certificados raiz não são publicados) e a assinatura de software
#       (algoritmo CEPESC, não público) também não.
#   N4. Ritmo de votação no log: hora de cada voto computado, votos antes da
#       abertura e depois do encerramento, intervalos curtos, lacunas.
#   N5. Liberações sem biometria: eleitores habilitados por biometria e por
#       dado biográfico (ano de nascimento), lidos do BU.
#
# Fonte: portal de dados abertos do TSE, "Resultados 2026 - Arquivos
# transmitidos para totalização" (um ZIP por UF e turno, só arquivos com
# situação "Totalizado"). Se o ZIP de um turno ainda não estiver publicado,
# use --modo api para baixar uma amostra de seções direto do portal Resultados.
#
# A decodificação ASN.1, a recontagem e a verificação de assinatura usam o
# código do projeto DataUrnas-BR (licença MIT, pasta urna_vendor/), que segue
# as especificações publicadas pelo TSE.
#
# COMO RODAR (no Prompt de Comando ou PowerShell, dentro da pasta do projeto):
#   py -m pip install asn1tools cryptography py7zr
#   py 09_arquivos_urna.py                       (todas as UFs, 1º e 2º turnos)
#   py 09_arquivos_urna.py --ufs AC RR --turnos 1  (teste rápido)
#   py 09_arquivos_urna.py --modo api --amostra 3000 --turnos 2
# Pode interromper e rodar de novo: UFs já processadas são puladas.
# Espaço: ~95 GB de ZIPs por turno (use --apagar-zip para apagar cada ZIP
# depois de processado).
# =============================================================================
from __future__ import annotations

import argparse
import concurrent.futures as cf
import csv
import hashlib
import json
import os
import random
import re
import sys
import time
import urllib.error
import urllib.request
import zipfile
from collections import Counter, defaultdict
from pathlib import Path

AQUI = Path(__file__).resolve().parent
sys.path.insert(0, str(AQUI / "urna_vendor" / "src"))

try:
    from dataurnas.analyzer.rdv_recontagem import (EVENTO_VOTO, contagens_bu,
                                                    processar_secao, texto_log)
    from dataurnas.parsers.bu import BUParser
    from dataurnas.parsers.signature import verificar_vsc
except ImportError as e:  # pacote ausente
    sys.exit(f"Faltam pacotes Python ({e}). Rode: py -m pip install asn1tools cryptography py7zr")

UFS = ["AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA",
       "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO", "ZZ"]
# Códigos do TSE para 2026 (conferidos nos arquivos reais: pleito no nome do arquivo,
# eleição dentro do BU)
PLEITO = {1: 3220, 2: 3221}
ELEICAO_PRES = {1: 6257, 2: 6258}
DATA = {1: "04/10/2026", 2: "25/10/2026"}
MODELOS = {9: "UE2009", 10: "UE2010", 11: "UE2011", 13: "UE2013", 15: "UE2015",
           20: "UE2020", 22: "UE2022"}
URL_ZIP = ("https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2026/arqurnatot/"
           "bu_imgbu_logjez_rdv_vscmr_2026_{t}t_{uf}.zip")
CKAN = ("https://dadosabertos.tse.jus.br/api/3/action/package_show?"
        "id=resultados-2026-arquivos-transmitidos-para-totalizacao")
API = "https://resultados.tse.jus.br/oficial/ele2026/arquivo-urna/{p}"
UA = {"User-Agent": "pesquisa-academica-forense-eleitoral/1.0"}

# o03220ac0101500020088-bu.dat -> pleito, uf, município, zona, seção, tipo, extensão
RE_NOME = re.compile(r"o(\d{5})([a-z]{2})(\d{5})(\d{4})(\d{4})-([a-z]+)\.(dat|jez|vsc)$")
TIPOS = {("bu", "dat"): "bu", ("busa", "dat"): "busa", ("rdv", "dat"): "rdv",
         ("log", "jez"): "log", ("logsa", "jez"): "logsa", ("vota", "vsc"): "vsc",
         ("sa", "vsc"): "vscsa"}

CAMPOS_SECAO = [
    "turno", "uf", "cd_mun", "zona", "secao", "status", "motivo", "origem_bu", "tipo_urna",
    "modelo", "aptos_bu", "comparecimento_bu", "habilitados_biometria", "habilitados_biografia",
    "hora_abertura_bu", "hora_encerramento_bu", "rdv_confere", "registros_rdv", "n_cargos",
    "estrutura_rdv_ok", "origem_rdv", "hash_bu_ok", "hash_rdv_ok", "hash_log_ok",
    "assinatura_valida", "assinatura_algoritmo", "assinatura_motivos", "origem_log",
    "votos_log", "diferenca_log_bu", "votos_outras_datas", "n_urnas_log", "aberturas",
    "hora_pronta", "hora_encerramento", "primeiro_voto", "ultimo_voto", "fuso_offset",
    "votos_antes_8h", "votos_apos_17h", "votos_antes_pronta", "votos_apos_encerramento",
    "max_votos_minuto", "minuto_pico", "intervalo_min_s", "intervalo_mediana_s",
    "intervalos_lt_10s", "intervalos_lt_5s", "maior_lacuna_min", "lacunas_60min",
    "inversoes_relogio", "ajustes_hora", "flags_log"]
CAMPOS_VOTOS = ["turno", "uf", "cd_mun", "zona", "secao", "fonte", "tipo", "numero", "votos"]
CAMPOS_HORA = ["turno", "uf", "cd_mun", "zona", "secao", "hora_local", "votos"]
CAMPOS_CARGO = ["turno", "uf", "cd_mun", "zona", "secao", "id_eleicao", "cargo", "codigo_cargo",
                "qtd_escolhas", "registros_rdv", "comparecimento_bu", "votos_rdv", "votos_bu",
                "confere", "detalhes"]


# ---------------------------------------------------------------------------
# Processamento de uma seção (bytes em memória) - roda nos processos do pool
# ---------------------------------------------------------------------------
def _sha(b: bytes) -> bytes:
    return hashlib.sha512(b).digest()


def votos_por_hora(txt: str, data: str) -> Counter:
    c: Counter = Counter()
    for ln in txt.split("\n"):
        if ln.startswith(data) and EVENTO_VOTO in ln:
            c[ln[11:13]] += 1
    return c


def processar(turno: int, ident: dict, arquivos: dict[str, tuple[str, bytes]],
              verificar_assinatura: bool = True) -> dict:
    base = {"turno": turno, **ident}
    dados = {k: v for k, v in arquivos.items() if k in ("bu", "busa", "rdv", "log", "logsa")}
    out = processar_secao(ident, dados, pleito=PLEITO[turno], data_eleicao=DATA[turno])
    s = {**base, **{k: v for k, v in out["secao"].items() if k in CAMPOS_SECAO}}
    s["estrutura_rdv_ok"] = out["secao"].get("estrutura_ok")
    s["tipo_urna"] = out["secao"].get("tipo_urna_rdv")
    votos: list[dict] = []
    # BU: aptos, biometria, horários e votos de Presidente
    tipo_bu = "bu" if "bu" in dados else ("busa" if "busa" in dados else None)
    if tipo_bu:
        try:
            bu = BUParser().parse_bytes(dados[tipo_bu][1])
            s.update(habilitados_biometria=bu.qtd_eleitores_comp_biometrico,
                     habilitados_biografia=bu.qtd_eleitores_lib_codigo,
                     hora_abertura_bu=bu.hora_abertura, hora_encerramento_bu=bu.hora_encerramento,
                     tipo_urna=s.get("tipo_urna") or bu.tipo_urna)
            for r in bu.resultados_por_eleicao:
                if int(r.id_eleicao) == ELEICAO_PRES[turno]:
                    s["aptos_bu"] = r.eleitores_aptos
            for (id_el, cargo), v in contagens_bu(bu).items():
                if cargo == 1 and id_el == ELEICAO_PRES[turno]:
                    for (tp, num), q in v["votos"].items():
                        votos.append({**base, "fonte": "bu", "tipo": tp, "numero": num, "votos": q})
        except Exception as e:  # noqa: BLE001 - registrado no motivo
            s["motivo"] = (s.get("motivo") or "") + f" | BU extra: {e!r}"[:200]
    for chave, q in out["presidente"].items():
        tp, _, num = chave.partition(":")
        votos.append({**base, "fonte": "rdv", "tipo": tp, "numero": num or None, "votos": q})
    # Log: métricas de horário (N4) e votos por hora
    t = out.get("tempo") or {}
    for k in CAMPOS_SECAO:
        if k in t and k not in ("turno", "uf", "cd_mun", "zona", "secao"):
            s[k] = t[k]
    s["flags_log"] = ";".join(t.get("flags", [])) or None
    horas: list[dict] = []
    tipo_log = "log" if "log" in dados else None
    if tipo_log:
        try:
            txt = texto_log(dados[tipo_log][1]) or ""
            horas = [{**base, "hora_local": h, "votos": n}
                     for h, n in sorted(votos_por_hora(txt, DATA[turno]).items())]
        except Exception:  # noqa: BLE001
            pass
    # Assinatura e hashes (N2)
    vsc = arquivos.get("vsc") or arquivos.get("vscsa")
    if vsc is None:
        s["assinatura_motivos"] = "arquivo de assinatura ausente"
    else:
        r = verificar_vsc(vsc[1], arquivos_verificar=set()) if verificar_assinatura else None
        if r is None:  # só hashes, sem verificar a assinatura
            from dataurnas.parsers.signature import parse_vsc
            try:
                v = parse_vsc(vsc[1])
                hs = {a["nome"]: a["hash"] for a in v["hw"]["arquivos"] + v["sw"]["arquivos"]}
                r = {"hashes": hs, "modelo_cod": v["modelo_cod"], "assinatura_valida": None,
                     "algoritmo": None, "motivos": ["assinatura não verificada (--sem-assinatura)"],
                     "falhas": []}
            except Exception as e:  # noqa: BLE001
                r = {"hashes": {}, "modelo_cod": None, "assinatura_valida": False,
                     "algoritmo": None, "motivos": [], "falhas": [f"ilegível: {e}"]}
        s["assinatura_valida"] = r["assinatura_valida"]
        s["assinatura_algoritmo"] = r["algoritmo"]
        s["assinatura_motivos"] = "; ".join(r["motivos"] + r["falhas"])[:300] or None
        s["modelo"] = MODELOS.get(r["modelo_cod"], r["modelo_cod"])
        for tp, campo in (("bu", "hash_bu_ok"), ("busa", "hash_bu_ok"), ("rdv", "hash_rdv_ok"),
                          ("log", "hash_log_ok"), ("logsa", "hash_log_ok")):
            if tp in arquivos:
                nome, raw = arquivos[tp]
                esperado = r["hashes"].get(nome)
                s[campo] = None if esperado is None else (_sha(raw) == esperado)
    cargos = [{"turno": turno, **{k: ln.get(k) for k in CAMPOS_CARGO if k != "turno"}}
              for ln in out["linhas"]]
    achados = [{"turno": turno, **a} for a in out["achados"]]
    return {"secao": s, "votos": votos, "horas": horas, "cargos": cargos, "achados": achados}


# ---------------------------------------------------------------------------
# Modo ZIP: um ZIP por UF e turno
# ---------------------------------------------------------------------------
_ZIP = None
_TURNO = None
_ASSIN = True


def _init_zip(caminho: str, turno: int, assin: bool) -> None:
    global _ZIP, _TURNO, _ASSIN
    _ZIP = zipfile.ZipFile(caminho)
    _TURNO, _ASSIN = turno, assin


def _tarefa_zip(item):
    stem, membros = item
    m = RE_NOME.search(stem + "-bu.dat")
    ident = {"uf": m.group(2), "cd_mun": m.group(3), "zona": m.group(4), "secao": m.group(5)}
    try:
        arquivos = {tp: (Path(nome).name, _ZIP.read(nome)) for tp, nome in membros.items()}
        return processar(_TURNO, ident, arquivos, _ASSIN)
    except Exception as e:  # noqa: BLE001 - uma seção ruim não derruba a UF
        return {"secao": {"turno": _TURNO, **ident, "status": "erro", "motivo": repr(e)[:300]},
                "votos": [], "horas": [], "cargos": [], "achados": []}


def _get(url: str, destino: Path | None = None, tentativas: int = 8):
    """GET com retomada (Range) e novas tentativas. Sem destino, devolve bytes."""
    for i in range(tentativas):
        try:
            if destino is None:
                with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60) as r:
                    return r.read()
            parcial = destino.with_suffix(destino.suffix + ".parcial")
            ja = parcial.stat().st_size if parcial.exists() else 0
            req = urllib.request.Request(url, headers={**UA, "Range": f"bytes={ja}-"} if ja else UA)
            with urllib.request.urlopen(req, timeout=120) as r:
                total = int(r.headers.get("Content-Length", 0)) + ja
                modo = "ab" if r.status == 206 else "wb"
                with open(parcial, modo) as f:
                    baixado, t0 = (ja if modo == "ab" else 0), time.time()
                    while bloco := r.read(4 * 1024 * 1024):
                        f.write(bloco)
                        baixado += len(bloco)
                        if time.time() - t0 > 15:
                            print(f"    {baixado / 1e9:.2f} de {total / 1e9:.2f} GB", flush=True)
                            t0 = time.time()
            parcial.replace(destino)
            return destino
        except urllib.error.HTTPError as e:
            if e.code in (404, 403):
                return None
            if e.code == 416 and destino is not None:  # já completo
                destino.with_suffix(destino.suffix + ".parcial").replace(destino)
                return destino
            espera = min(60, 2 ** i)
        except (urllib.error.URLError, TimeoutError, ConnectionError, OSError):
            espera = min(60, 2 ** i)
        print(f"    falha de rede; nova tentativa em {espera}s", flush=True)
        time.sleep(espera)
    return None


_CKAN_CACHE: list | None = None


def url_do_zip(turno: int, uf: str) -> str:
    global _CKAN_CACHE
    if _CKAN_CACHE is None:
        try:
            _CKAN_CACHE = json.loads(_get(CKAN, tentativas=2) or b"{}").get("result", {}).get("resources", [])
        except Exception:  # noqa: BLE001
            _CKAN_CACHE = []
    alvo = f"_{turno}t_{uf}.zip".lower()
    for r in _CKAN_CACHE:
        if str(r.get("url", "")).lower().endswith(alvo):
            return r["url"]
    return URL_ZIP.format(t=turno, uf=uf)


def zip_integro(caminho: Path, url: str) -> bool:
    try:
        with zipfile.ZipFile(caminho) as z:
            z.namelist()
    except (zipfile.BadZipFile, OSError):
        return False
    sha = _get(url + ".sha512", tentativas=2)
    if sha:
        esperado = sha.decode("ascii", "ignore").split()[0].lower()
        h = hashlib.sha512()
        with open(caminho, "rb") as f:
            while b := f.read(8 * 1024 * 1024):
                h.update(b)
        if h.hexdigest() != esperado:
            print("    SHA-512 do ZIP não confere com o publicado pelo TSE", flush=True)
            return False
        print("    SHA-512 do ZIP confere com o publicado pelo TSE", flush=True)
    return True


def membros_por_secao(z: zipfile.ZipFile, pleito: int) -> dict[str, dict[str, str]]:
    grupos: dict[str, dict[str, str]] = defaultdict(dict)
    for nome in z.namelist():
        m = RE_NOME.search(nome.lower())
        if not m or int(m.group(1)) != pleito:
            continue
        tp = TIPOS.get((m.group(6), m.group(7)))
        if tp:
            stem = nome.lower()[m.start():].split("-")[0]
            grupos[stem][tp] = nome
    return grupos


class Saida:
    """Grava os CSVs de uma UF/turno; só renomeia para o nome final no fim."""

    def __init__(self, pasta: Path, sufixo: str):
        self.final = {k: pasta / f"{k}_{sufixo}.csv" for k in ("secoes", "votos", "horas", "cargos")}
        self.final["achados"] = pasta / f"achados_{sufixo}.jsonl"
        self.tmp = {k: v.with_suffix(v.suffix + ".tmp") for k, v in self.final.items()}
        self.f = {k: open(v, "w", newline="", encoding="utf-8") for k, v in self.tmp.items()}
        self.w = {k: csv.DictWriter(self.f[k], fieldnames=c, extrasaction="ignore", delimiter=";")
                  for k, c in (("secoes", CAMPOS_SECAO), ("votos", CAMPOS_VOTOS),
                               ("horas", CAMPOS_HORA), ("cargos", CAMPOS_CARGO))}
        for w in self.w.values():
            w.writeheader()
        self.n = Counter()

    def gravar(self, r: dict) -> None:
        self.w["secoes"].writerow(r["secao"])
        self.w["votos"].writerows(r["votos"])
        self.w["horas"].writerows(r["horas"])
        self.w["cargos"].writerows(r["cargos"])
        for a in r["achados"]:
            self.f["achados"].write(json.dumps(a, ensure_ascii=False, default=str) + "\n")
        s = r["secao"]
        self.n["secoes"] += 1
        self.n["rdv_diverge"] += s.get("rdv_confere") is False and s.get("status") == "ok"
        self.n["hash_falha"] += any(s.get(k) is False for k in ("hash_bu_ok", "hash_rdv_ok", "hash_log_ok"))
        self.n["assinatura_invalida"] += s.get("assinatura_valida") is False
        self.n["erro"] += s.get("status") not in ("ok", None)

    def fechar(self) -> None:
        for f in self.f.values():
            f.close()
        for k in self.final:
            self.tmp[k].replace(self.final[k])


def modo_zip(args) -> None:
    pasta_zip = Path(args.pasta) / "zip"
    pasta_out = Path(args.pasta) / "saida"
    pasta_zip.mkdir(parents=True, exist_ok=True)
    pasta_out.mkdir(parents=True, exist_ok=True)
    resumo = []
    for turno in args.turnos:
        for uf in args.ufs:
            sufixo = f"t{turno}_{uf}"
            if (pasta_out / f"secoes_{sufixo}.csv").exists() and not args.refazer:
                print(f"[{sufixo}] já processado; pulando", flush=True)
                continue
            url = url_do_zip(turno, uf)
            destino = pasta_zip / Path(url).name
            print(f"[{sufixo}] {url}", flush=True)
            if not destino.exists() or not zip_integro(destino, url):
                if destino.exists():
                    destino.unlink()
                if _get(url, destino) is None:
                    print(f"[{sufixo}] ZIP não disponível no TSE (ainda não publicado?). "
                          "Tente --modo api para uma amostra.", flush=True)
                    resumo.append({"turno": turno, "uf": uf, "situacao": "zip indisponível"})
                    continue
                if not zip_integro(destino, url):
                    print(f"[{sufixo}] ZIP corrompido; rode de novo", flush=True)
                    resumo.append({"turno": turno, "uf": uf, "situacao": "zip corrompido"})
                    continue
            with zipfile.ZipFile(destino) as z:
                grupos = membros_por_secao(z, PLEITO[turno])
            print(f"[{sufixo}] {len(grupos)} seções no ZIP; processando com {args.processos} processos",
                  flush=True)
            out = Saida(pasta_out, sufixo)
            t0 = time.time()
            with cf.ProcessPoolExecutor(args.processos, initializer=_init_zip,
                                        initargs=(str(destino), turno, not args.sem_assinatura)) as ex:
                for i, r in enumerate(ex.map(_tarefa_zip, sorted(grupos.items()), chunksize=32), 1):
                    out.gravar(r)
                    if i % 2000 == 0:
                        print(f"    {i}/{len(grupos)} seções ({(time.time() - t0) / 60:.1f} min)", flush=True)
            out.fechar()
            print(f"[{sufixo}] concluído: {dict(out.n)}", flush=True)
            resumo.append({"turno": turno, "uf": uf, "situacao": "ok", **out.n})
            if args.apagar_zip:
                destino.unlink()
    with open(pasta_out / f"resumo_execucao_{time.strftime('%Y%m%d_%H%M%S')}.json", "w", encoding="utf-8") as f:
        json.dump(resumo, f, ensure_ascii=False, indent=1)


# ---------------------------------------------------------------------------
# Modo API: amostra aleatória estratificada por UF, direto do portal Resultados
# ---------------------------------------------------------------------------
def _escolher_hash(aux: dict):
    hs = [h for h in (aux.get("hashes") or []) if h.get("hash") and h.get("hash") != "0"]
    if not hs:
        return None, []
    tot = [h for h in hs if str(h.get("st", "")).lower().startswith("totalizad")]
    h = tot[-1] if tot else hs[-1]
    nomes = [a.get("nm") for a in (h.get("arq") or []) if isinstance(a, dict)] + list(h.get("nmarq") or [])
    return h["hash"], [n for n in nomes if n]


def _secao_api(turno: int, sec: dict) -> dict:
    p = PLEITO[turno]
    uf, mun, zona, secao = sec["uf"], sec["cd_mun"], sec["zona"], sec["secao"]
    base = f"{API.format(p=p)}/dados/{uf}/{mun}/{zona}/{secao}"
    ident = {"uf": uf, "cd_mun": mun, "zona": zona, "secao": secao}
    aux = _get(f"{base}/p{p:06d}-{uf}-m{mun}-z{zona}-s{secao}-aux.json")
    if not aux:
        return {"secao": {"turno": turno, **ident, "status": "sem_aux"}, "votos": [], "horas": [],
                "cargos": [], "achados": []}
    h, nomes = _escolher_hash(json.loads(aux))
    arquivos = {}
    for nome in nomes:
        m = RE_NOME.search(nome.lower())
        tp = TIPOS.get((m.group(6), m.group(7))) if m else None
        if tp:
            b = _get(f"{base}/{h}/{nome}")
            if b:
                arquivos[tp] = (nome, b)
    if not arquivos:
        return {"secao": {"turno": turno, **ident, "status": "sem_arquivos"}, "votos": [], "horas": [],
                "cargos": [], "achados": []}
    return processar(turno, ident, arquivos, True)


def modo_api(args) -> None:
    pasta_out = Path(args.pasta) / "saida"
    pasta_out.mkdir(parents=True, exist_ok=True)
    rnd = random.Random(2026)
    for turno in args.turnos:
        p = PLEITO[turno]
        secoes = []
        for uf in args.ufs:
            u = uf.lower()
            cs = _get(f"{API.format(p=p)}/config/{u}/{u}-p{p:06d}-cs.json")
            if not cs:
                print(f"[t{turno} {uf}] configuração de seções indisponível", flush=True)
                continue
            for abr in json.loads(cs).get("abr", []):
                for mu in abr.get("mu", []):
                    for zon in mu.get("zon", []):
                        for s in zon.get("sec", []):
                            if s.get("nsp"):   # seção agregada a outra: sem arquivos próprios
                                continue
                            secoes.append({"uf": abr.get("cd", u).lower(), "cd_mun": mu["cd"],
                                           "zona": zon["cd"], "secao": s["ns"]})
        por_uf = defaultdict(list)
        for s in secoes:
            por_uf[s["uf"]].append(s)
        total = len(secoes)
        amostra = []
        for uf, lst in por_uf.items():   # proporcional, com mínimo de 30 por UF
            k = min(len(lst), max(30, round(args.amostra * len(lst) / max(total, 1))))
            amostra += rnd.sample(lst, k)
        print(f"[t{turno}] {total} seções; amostra de {len(amostra)}", flush=True)
        out = Saida(pasta_out, f"t{turno}_amostra_api")
        with cf.ThreadPoolExecutor(args.processos) as ex:
            for i, r in enumerate(ex.map(lambda s: _secao_api(turno, s), amostra), 1):
                out.gravar(r)
                if i % 200 == 0:
                    print(f"    {i}/{len(amostra)}", flush=True)
        out.fechar()
        print(f"[t{turno}] concluído: {dict(out.n)}", flush=True)


def main() -> None:
    ap = argparse.ArgumentParser(description="Testes com os arquivos de urna (N1, N2, N4, N5)")
    ap.add_argument("--ufs", nargs="+", default=UFS, type=str.upper)
    ap.add_argument("--turnos", nargs="+", default=[1, 2], type=int, choices=[1, 2])
    ap.add_argument("--modo", choices=["zip", "api"], default="zip")
    ap.add_argument("--amostra", type=int, default=3000, help="tamanho da amostra no modo api")
    ap.add_argument("--pasta", default=str(Path("tse_2026") / "urna"))
    ap.add_argument("--processos", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    ap.add_argument("--sem-assinatura", action="store_true",
                    help="confere só os hashes (mais rápido), sem verificar a assinatura")
    ap.add_argument("--apagar-zip", action="store_true", help="apaga cada ZIP depois de processado")
    ap.add_argument("--refazer", action="store_true", help="reprocessa UFs já concluídas")
    args = ap.parse_args()
    t0 = time.time()
    (modo_api if args.modo == "api" else modo_zip)(args)
    print(f"Fim em {(time.time() - t0) / 60:.1f} min. Próximo passo: 10_testes_arquivos_urna.R", flush=True)


if __name__ == "__main__":
    main()

# Monta o painel por município (um único HTML com os dados embutidos).
# Uso: python3 montar_painel_municipios.py <pasta painel-dados> <clusters.json> <saida.html>
# Ordem: (1) este script grava dados_municipios.json; (2) Rscript scripts/16_clusters_municipios.R
#        dados_municipios.json 3,8 grava clusters.json; (3) este script de novo monta o HTML.
# Fonte: modelo.html (página) e decisao_k.html (texto da escolha de k), na mesma pasta.
import json, glob, sys, os
import sys
D = sys.argv[1] if len(sys.argv) > 1 else "painel_dados"          # pasta com o conteúdo do branch painel-dados
CL = sys.argv[2] if len(sys.argv) > 2 else "clusters.json"         # saída do 16_clusters_municipios.R
SAIDA = sys.argv[3] if len(sys.argv) > 3 else "index.html"
R = json.load(open(f"{D}/resumo.json")); C = json.load(open(f"{D}/candidatos.json")); M = json.load(open(f"{D}/municipios.json"))
A = lambda x: x if isinstance(x, list) else [x]
testes = R["testes"]; nb = len(testes)
PER = ["fem","i1624","i60","sup","baixa","bio","rinf"]
CEN = ["renda","pp","ind","analf","esg"]
out = {"testes": [{k: t[k] for k in ("cod","nome","descricao")} for t in testes],
       "territorios": R["territorios"],
       "cand": {t: [{"num": c["num"], "nome": c["nome"], "esp": c["especial"]} for c in C["candidatos"][t]] for t in ("1","2")},
       "ufs": {}, "gerado": R["gerado_em"]}
check = {"fem": [0,0], "renda": [0,0]}
for f in sorted(glob.glob(f"{D}/uf_*.json")):
    uf = os.path.basename(f)[3:5].upper()
    u = json.load(open(f)); c = json.load(open(f"{D}/cand_uf_{uf.lower()}.json"))
    cod = A(u["municipios"]["cod"]); nome = A(u["municipios"]["nome"]); n_m = len(cod)
    S = {k: A(v) for k, v in u["secoes"].items()}; L = {k: A(v) for k, v in u["locais"].items()}
    pos = {(m,z,s): j for j,(m,z,s) in enumerate(zip(c["mun"], c["zona"], c["secao"]))}
    nc1, nc2 = len(c["v1"]), len(c["v2"])
    z = lambda: [0]*n_m
    acc = {k: z() for k in ["sec","apt","cmp1","cmp2","wp","wr","rpp","wc","qual"] + ["p_"+k for k in PER] + ["c_"+k for k in CEN]}
    v1 = [z() for _ in range(nc1)]; v2 = [z() for _ in range(nc2)]
    fl = [z() for _ in range(nb)]; terr = [z() for _ in range(6)]; locs = [set() for _ in range(n_m)]
    for i in range(len(S["mun"])):
        m = S["mun"][i]; j = pos[(cod[m], S["zona"][i], S["secao"][i])]
        acc["sec"][m] += 1
        ap2 = c["apt2"][j] or 0; acc["apt"][m] += ap2
        acc["cmp1"][m] += c["cmp1"][j] or 0; acc["cmp2"][m] += c["cmp2"][j] or 0
        for k in range(nc1): v1[k][m] += c["v1"][k][j] or 0
        for k in range(nc2): v2[k][m] += c["v2"][k][j] or 0
        ap = S["aptos"][i]
        if ap and S["fem"][i] is not None:
            acc["wp"][m] += ap
            for k in PER: acc["p_"+k][m] += (S[k][i] or 0) * ap / 1000
            if S["rpp"][i] is not None and S["rinf"][i]:
                w = S["rinf"][i] * ap / 1000; acc["wr"][m] += w; acc["rpp"][m] += S["rpp"][i] / 1000 * w
        l = S["local"][i]; locs[m].add(l); terr[L["terr"][l] if L["terr"][l] is not None else 5][m] += 1
        w = ap or ap2
        if L["renda"][l] is not None and w:
            acc["wc"][m] += w
            for k in CEN: acc["c_"+k][m] += (L[k][l] or 0) * w / (1 if k == "renda" else 1000)
        f_ = S["flags"][i]
        if f_:
            acc["qual"][m] += 1
            for b in range(nb):
                if f_ >> b & 1: fl[b][m] += 1
    mj = M[uf]; ix = {cc: k for k, cc in enumerate(A(mj["CD_MUNICIPIO"]))}
    g = lambda col, m: A(mj[col])[ix[cod[m]]] if cod[m] in ix else None
    rd = lambda x, d=4: None if x is None else round(x, d)
    U = {"cod": cod, "nome": nome, "sec": acc["sec"], "loc": [len(s) for s in locs], "apt": acc["apt"], "cmp1": acc["cmp1"], "cmp2": acc["cmp2"],
         "v1": v1, "v2": v2, "wp": acc["wp"], "wr": [round(x) for x in acc["wr"]], "wc": acc["wc"],
         "p": {k: [rd(a / w) if w else None for a, w in zip(acc["p_"+k], acc["wp"])] for k in PER},
         "rpp": [rd(a / w) if w else None for a, w in zip(acc["rpp"], acc["wr"])],
         "c": {k: [rd(a / w, 0 if k == "renda" else 4) if w else None for a, w in zip(acc["c_"+k], acc["wc"])] for k in CEN},
         "terr": terr, "fl": fl, "qual": acc["qual"],
         "pop": [g("pop", m) for m in range(n_m)], "pop15": [g("pop15", m) for m in range(n_m)], "razao": [g("razao", m) for m in range(n_m)],
         "ibge": [g("cod_ibge", m) for m in range(n_m)], "n12": [g("n12_dip", m) for m in range(n_m)]}
    out["ufs"][uf] = U
    check["fem"][0] += sum(acc["p_fem"]); check["fem"][1] += sum(acc["wp"])
    check["renda"][0] += sum(acc["c_renda"]); check["renda"][1] += sum(acc["wc"])
    tot1 = [sum(x) for x in v1]; assert tot1 == C["ufs"][uf]["1"], uf
print("fem", check["fem"][0]/check["fem"][1], "vs", R["brasil"]["tse"]["fem"])
print("renda", check["renda"][0]/check["renda"][1], "vs", R["brasil"]["censo"]["renda"])
print("muns", sum(len(u["cod"]) for u in out["ufs"].values()), "secoes", sum(sum(u["sec"]) for u in out["ufs"].values()))

# ---- território das seções marcadas por teste -----------------------------------
tt = [[0]*6 for _ in range(nb)]; base = [0]*6
# ---- lista das seções marcadas ----------------------------------------------------
sm = []; locs = {}
for f in sorted(glob.glob(f"{D}/uf_*.json")):
    uf = os.path.basename(f)[3:5].upper()
    u = json.load(open(f)); c = json.load(open(f"{D}/cand_uf_{uf.lower()}.json"))
    S = {k: A(v) for k, v in u["secoes"].items()}; L = {k: A(v) for k, v in u["locais"].items()}; cod = A(u["municipios"]["cod"])
    pos = {(m,z,s): j for j,(m,z,s) in enumerate(zip(c["mun"], c["zona"], c["secao"]))}
    for i, fl in enumerate(S["flags"]):
        l = S["local"][i]; t = L["terr"][l] if L["terr"][l] is not None else 5; base[t] += 1
        if not fl: continue
        for b in range(nb):
            if fl >> b & 1: tt[b][t] += 1
        m = S["mun"][i]; j = pos[(cod[m], S["zona"][i], S["secao"][i])]
        key = (uf, l); locs.setdefault(key, (len(locs), L["nome"][l]))
        lu, fv = c["v2"][0][j] or 0, c["v2"][1][j] or 0
        sm.append([uf, m, S["zona"][i], S["secao"][i], locs[key][0], t, c["apt2"][j], c["cmp1"][j], c["cmp2"][j],
                   round(lu/(lu+fv), 4) if lu+fv else None, S["zcen"][i], fl, c["apt1"][j]])
out["tterr"] = tt; out["tbase"] = base
nomes_loc = [None]*len(locs)
for (uf, l), (k, nome) in locs.items(): nomes_loc[k] = nome
out["sm"] = {"cols": ["uf","m","z","s","loc","terr","apt","cmp1","cmp2","l2","z15","fl","apt1"], "rows": sm, "locais": nomes_loc}

# dados por município sem os agrupamentos: entrada do 16_clusters_municipios.R
json.dump(out, open("dados_municipios.json", "w", encoding="utf-8"), separators=(",", ":"), ensure_ascii=False)
if not os.path.exists(CL):
    sys.exit(f"Gravado dados_municipios.json. Rode agora: Rscript 16_clusters_municipios.R dados_municipios.json 3,8 e depois este script de novo.")

# ---- agrupamentos (16_clusters_municipios.R) -------------------------------------
cl = json.load(open(CL, encoding="utf-8"))
cl["blocos"] = ["Votação"]*6 + ["Perfil do eleitorado (TSE)"]*6 + ["Censo 2022"]*5 + ["Território"]*4 + ["Porte"]
cl["k_recomendado"] = 3
cl["decisao"] = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "decisao_k.html"), encoding="utf-8").read()
out["cl"] = cl

s = json.dumps(out, separators=(",", ":"), ensure_ascii=False)
modelo = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "modelo.html"), encoding="utf-8").read()
open(SAIDA, "w", encoding="utf-8").write(modelo.replace("__DADOS__", s.replace("</", "<\\/")))
print("Gravado", SAIDA, f"({len(s.encode())/1e6:.1f} MB de dados)")

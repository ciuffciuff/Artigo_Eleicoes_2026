# =============================================================================
# 10_testes_arquivos_urna.R - Resultados dos novos testes 1, 2, 4 e 5
#
# Lê o que o 09 extraiu dos arquivos de urna (tse_2026/urna/saida) e produz:
#   N1. Recontagem: RDV x BU (cargo a cargo) e BU x CSV oficial totalizado
#       (Presidente, seção a seção). Esperado: 0 divergências.
#   N2. Integridade: SHA-512 de BU, RDV e log x valor assinado pela urna, e
#       validade da assinatura de hardware, por modelo de urna.
#   N4. Ritmo de votação: curva de votos por hora (horário de Brasília),
#       votos fora da sequência abertura/encerramento, picos por minuto,
#       intervalos curtos; z robusto dentro do município.
#   N5. Habilitação sem biometria: taxa por seção comparada com o município
#       e cruzada com o voto (efeito fixo de município).
#   + conferência do modelo da urna: assinatura x detalhe do TSE (N6).
# Pré-requisito: 09 rodado (ao menos uma UF).
# =============================================================================
source("00_config.R")

z_limite <- 6
pasta_out <- file.path(pasta_base, "urna", "saida")

ler_saida <- function(prefixo) {
  arqs <- list.files(pasta_out, sprintf("^%s_t[12]_.*\\.csv$", prefixo), full.names = TRUE)
  if (!length(arqs)) return(NULL)
  rbindlist(lapply(arqs, fread, sep = ";", colClasses = list(character = c("uf", "cd_mun", "zona", "secao")),
                   encoding = "UTF-8"), fill = TRUE)
}
para_chave <- function(d) {
  d[, `:=`(NR_TURNO = as.integer(turno), SG_UF = toupper(uf), CD_MUNICIPIO = as.integer(cd_mun),
           NR_ZONA = as.integer(zona), NR_SECAO = as.integer(secao))]
  d
}

etapa("Leitura e teste de carga")
secs <- ler_saida("secoes")
if (is.null(secs)) stop("Nada em ", pasta_out, ". Rode 09_arquivos_urna.R antes.")
secs <- para_chave(secs)
campos <- c("status", "motivo", "origem_bu", "tipo_urna", "modelo", "aptos_bu", "comparecimento_bu",
            "habilitados_biometria", "habilitados_biografia", "rdv_confere", "registros_rdv", "estrutura_rdv_ok",
            "hash_bu_ok", "hash_rdv_ok", "hash_log_ok", "assinatura_valida", "assinatura_algoritmo",
            "assinatura_motivos", "origem_log", "votos_log", "diferenca_log_bu", "votos_outras_datas",
            "votos_antes_8h", "votos_apos_17h", "votos_antes_pronta", "votos_apos_encerramento",
            "max_votos_minuto", "intervalos_lt_5s", "intervalos_lt_10s", "lacunas_60min",
            "inversoes_relogio", "fuso_offset", "flags_log")
for (cc in setdiff(campos, names(secs))) secs[, (cc) := NA]
dup <- secs[, .N, by = chave][N > 1]
if (nrow(dup)) { message("Aviso: ", nrow(dup), " seções repetidas (ZIP + amostra API); mantida a primeira."); secs <- unique(secs, by = chave) }
votos <- para_chave(ler_saida("votos"))
horas <- ler_saida("horas"); if (!is.null(horas)) horas <- para_chave(horas)
cargos <- ler_saida("cargos"); if (!is.null(cargos)) cargos <- para_chave(cargos)
print(secs[, .(UFs = uniqueN(SG_UF), secoes = .N), by = NR_TURNO])
para_lgl <- function(v) { v <- toupper(as.character(v)); fifelse(v == "TRUE", TRUE, fifelse(v == "FALSE", FALSE, NA)) }
for (cc in c("rdv_confere", "hash_bu_ok", "hash_rdv_ok", "hash_log_ok", "assinatura_valida", "estrutura_rdv_ok"))
  if (cc %in% names(secs)) set(secs, j = cc, value = para_lgl(secs[[cc]]))

pres <- ler_presidente()
lg <- largo_finalistas(pres)
lg[, mun := paste(SG_UF, CD_MUNICIPIO)]

# =============================================================================
# N1. Recontagem
# =============================================================================
etapa("N1. Recontagem RDV x BU e BU x resultado oficial")
cob <- merge(lg[, .(secoes_oficiais = .N), by = .(NR_TURNO, SG_UF)],
             secs[, .(secoes_com_arquivos = .N,
                      bu_urna = sum(origem_bu == "bu", na.rm = TRUE),
                      bu_sistema_apuracao = sum(origem_bu == "busa", na.rm = TRUE),
                      erro_leitura = sum(!status %in% c("ok", "sem_rdv"), na.rm = TRUE),
                      sem_rdv = sum(status == "sem_rdv", na.rm = TRUE),
                      rdv_confere = sum(rdv_confere %in% TRUE),
                      rdv_diverge = sum(rdv_confere %in% FALSE & status == "ok")),
                  by = .(NR_TURNO, SG_UF)], by = c("NR_TURNO", "SG_UF"), all = TRUE)
cb <- cob[, lapply(.SD, sum, na.rm = TRUE), by = NR_TURNO, .SDcols = setdiff(names(cob)[sapply(cob, is.numeric)], "NR_TURNO")]
print(cb)
salvar(cob, "N1_cobertura_recontagem.csv")
if (!is.null(cargos)) {
  rc <- cargos[, .(secao_cargo = .N, confere = sum(toupper(confere) == "TRUE")), by = .(NR_TURNO, cargo)]
  rc[, diverge := secao_cargo - confere]
  print(rc)
  salvar(rc, "N1_rdv_bu_por_cargo.csv")
  salvar(cargos[toupper(confere) != "TRUE"], "N1_rdv_bu_divergencias.csv")
}
# BU x CSV oficial (Presidente)
bu <- votos[fonte == "bu"]
bu[, NR_VOTAVEL := fifelse(tipo == "nominal", suppressWarnings(as.integer(numero)),
                    fifelse(tipo == "branco", 95L, fifelse(tipo %in% c("nulo", "cargoSemCandidato"), 96L, NA_integer_)))]
bu <- bu[, .(votos_bu = sum(votos)), by = c(chave, "NR_VOTAVEL")]
of <- pres[, .(votos_oficial = sum(QT_VOTOS)), by = c(chave, "NR_VOTAVEL")]
of[NR_VOTAVEL == 97L, NR_VOTAVEL := 96L]   # anulados contam como nulos no BU
of <- of[, .(votos_oficial = sum(votos_oficial)), by = c(chave, "NR_VOTAVEL")]
cmp <- merge(bu, of[secs[, ..chave], on = chave, nomatch = 0], by = c(chave, "NR_VOTAVEL"), all = TRUE)
for (cc in c("votos_bu", "votos_oficial")) set(cmp, which(is.na(cmp[[cc]])), cc, 0L)
cmp[, dif := votos_bu - votos_oficial]
div_sec <- cmp[, .(dif_abs = sum(abs(dif))), by = chave][dif_abs > 0]
tot <- cmp[, .(votos_bu = sum(as.numeric(votos_bu)), votos_oficial = sum(as.numeric(votos_oficial))), by = .(NR_TURNO, NR_VOTAVEL)]
tot[, diferenca := votos_bu - votos_oficial]
message(sprintf("  Seções comparadas: %d | com BU diferente do CSV oficial: %d",
                uniqueN(cmp[, ..chave]), nrow(div_sec)))
print(tot[order(NR_TURNO, -votos_oficial)])
salvar(tot, "N1_bu_x_oficial_totais.csv")
salvar(merge(div_sec, cmp, by = chave), "N1_bu_x_oficial_divergencias.csv")

# =============================================================================
# N2. Integridade e assinatura
# =============================================================================
etapa("N2. Hashes assinados e assinatura da urna")
n2 <- secs[, .(secoes = .N,
               hash_bu_ok = sum(hash_bu_ok %in% TRUE), hash_bu_falha = sum(hash_bu_ok %in% FALSE),
               hash_rdv_ok = sum(hash_rdv_ok %in% TRUE), hash_rdv_falha = sum(hash_rdv_ok %in% FALSE),
               hash_log_ok = sum(hash_log_ok %in% TRUE), hash_log_falha = sum(hash_log_ok %in% FALSE),
               assinatura_valida = sum(assinatura_valida %in% TRUE),
               assinatura_invalida = sum(assinatura_valida %in% FALSE),
               nao_verificada = sum(is.na(assinatura_valida))),
           by = .(NR_TURNO, modelo, assinatura_algoritmo)][order(NR_TURNO, modelo)]
print(n2)
salvar(n2, "N2_integridade_assinatura.csv")
salvar(secs[hash_bu_ok %in% FALSE | hash_rdv_ok %in% FALSE | hash_log_ok %in% FALSE | assinatura_valida %in% FALSE,
            c(chave, "modelo", "hash_bu_ok", "hash_rdv_ok", "hash_log_ok", "assinatura_valida",
              "assinatura_motivos"), with = FALSE], "N2_secoes_com_falha.csv")

# =============================================================================
# N4. Ritmo de votação no log
# =============================================================================
etapa("N4. Ritmo de votação (log da urna)")
num <- function(v) suppressWarnings(as.numeric(v))
for (cc in c("votos_log", "diferenca_log_bu", "votos_outras_datas", "votos_antes_8h", "votos_apos_17h",
             "votos_antes_pronta", "votos_apos_encerramento", "max_votos_minuto", "intervalos_lt_5s",
             "intervalos_lt_10s", "lacunas_60min", "inversoes_relogio", "fuso_offset", "comparecimento_bu",
             "habilitados_biometria", "habilitados_biografia", "aptos_bu"))
  if (cc %in% names(secs)) set(secs, j = cc, value = num(secs[[cc]]))
lg_urna <- secs[origem_log == "log"]
n4 <- lg_urna[, .(secoes_com_log = .N,
                  log_diferente_do_bu = sum(diferenca_log_bu != 0, na.rm = TRUE),
                  com_voto_antes_abertura = sum(votos_antes_pronta > 0, na.rm = TRUE),
                  com_voto_apos_encerramento = sum(votos_apos_encerramento > 0, na.rm = TRUE),
                  com_voto_antes_8h = sum(votos_antes_8h > 0, na.rm = TRUE),
                  votos_apos_17h_pct = round(100 * sum(votos_apos_17h, na.rm = TRUE) / sum(votos_log, na.rm = TRUE), 2),
                  com_intervalo_lt_5s = sum(intervalos_lt_5s > 0, na.rm = TRUE),
                  max_votos_em_1_minuto = max(max_votos_minuto, na.rm = TRUE)), by = NR_TURNO]
print(n4)
salvar(n4, "N4_ritmo_resumo.csv")
lg_urna <- merge(lg_urna, lg[, c(chave, "lula", "flavio", "mun"), with = FALSE], by = chave, all.x = TRUE)
lg_urna[, pct_lt10 := intervalos_lt_10s / pmax(votos_log - 1, 1)]
lg_urna[, `:=`(z_pico = (max_votos_minuto - median(max_votos_minuto, na.rm = TRUE)) /
                 pmax(mad(max_votos_minuto, na.rm = TRUE), 1),
               z_lt10 = (pct_lt10 - median(pct_lt10, na.rm = TRUE)) / pmax(mad(pct_lt10, na.rm = TRUE), 0.01)),
        by = .(NR_TURNO, mun)]
sin4 <- lg_urna[z_pico > z_limite | z_lt10 > z_limite | votos_antes_pronta > 0 | votos_apos_encerramento > 0 |
                (!is.na(diferenca_log_bu) & diferenca_log_bu != 0)]
message("  Seções sinalizadas no ritmo: ", nrow(sin4))
salvar(sin4[, c(chave, "votos_log", "comparecimento_bu", "diferenca_log_bu", "votos_antes_pronta",
                "votos_apos_encerramento", "max_votos_minuto", "intervalos_lt_5s", "intervalos_lt_10s",
                "z_pico", "z_lt10", "flags_log"), with = FALSE], "N4_secoes_sinalizadas.csv")
# votos depois das 17h x voto (dentro do município)
x <- lg_urna[votos_log > 30 & (lula + flavio) > 30]
if (nrow(x) > 100) {
  r17 <- rbindlist(lapply(split(x, by = "NR_TURNO"), function(d)
    fe_lm(logit_emp(d$lula, d$lula + d$flavio), data.frame(pct_apos_17h = 100 * d$votos_apos_17h / d$votos_log),
          fe = d$mun, cl = d$mun, w = d$lula + d$flavio)[, NR_TURNO := d$NR_TURNO[1]]))
  message("  Votos depois das 17h x % Lula (logit), dentro do município:")
  print(r17)
  salvar(r17, "N4_apos_17h_x_voto.csv")
}
if (!is.null(horas) && nrow(horas)) {
  hh <- merge(horas, secs[, c(chave, "fuso_offset"), with = FALSE], by = chave)
  hh[, hora_brasilia := suppressWarnings(as.integer(hora_local)) - fuso_offset]
  hh <- hh[!is.na(hora_brasilia)]
  curva <- hh[, .(votos = sum(votos)), by = .(NR_TURNO, hora_brasilia)][order(NR_TURNO, hora_brasilia)]
  curva[, pct := round(100 * votos / sum(votos), 3), by = NR_TURNO]
  salvar(curva, "N4_votos_por_hora.csv")
  png(file.path(pasta_saida, "N4_votos_por_hora.png"), width = 1000, height = 550)
  plot(NA, xlim = range(curva$hora_brasilia), ylim = c(0, max(curva$pct)), xlab = "Hora (Brasília)",
       ylab = "% dos votos do turno", main = "Votos computados por hora (log das urnas)")
  for (t in unique(curva$NR_TURNO)) lines(curva[NR_TURNO == t, hora_brasilia], curva[NR_TURNO == t, pct],
                                          type = "b", lwd = 2, col = t, pch = 19)
  abline(v = 17, lty = 2, col = "grey50")
  legend("topright", paste0(unique(curva$NR_TURNO), "º turno"), col = unique(curva$NR_TURNO), lwd = 2)
  dev.off()
}

# =============================================================================
# N5. Habilitação sem biometria
# =============================================================================
etapa("N5. Liberações sem biometria (BU)")
b5 <- merge(secs[!is.na(comparecimento_bu) & comparecimento_bu > 0 & origem_bu == "bu"],
            lg[, c(chave, "lula", "flavio", "mun"), with = FALSE], by = chave)
b5[, `:=`(tx_biografia = habilitados_biografia / comparecimento_bu,
          tx_sem_digital = pmax(comparecimento_bu - habilitados_biometria, 0) / comparecimento_bu)]
b5[, z_bio := (tx_biografia - median(tx_biografia, na.rm = TRUE)) / pmax(mad(tx_biografia, na.rm = TRUE), 0.01),
   by = .(NR_TURNO, mun)]
res5 <- b5[, .(secoes = .N, tx_biografia_mediana_pct = round(100 * median(tx_biografia, na.rm = TRUE), 2),
               tx_sem_digital_mediana_pct = round(100 * median(tx_sem_digital, na.rm = TRUE), 2),
               p99_biografia_pct = round(100 * quantile(tx_biografia, 0.99, na.rm = TRUE), 2),
               sinalizadas = sum(z_bio > z_limite, na.rm = TRUE)), by = NR_TURNO]
print(res5)
arq_res <- file.path(pasta_saida, "07_residuos_secao.csv")
if (file.exists(arq_res)) {        # controla pela proporção de eleitores com biometria cadastrada
  pb <- fread(arq_res, select = c(sec, "p_bio"))
  b5 <- merge(b5, pb, by = sec, all.x = TRUE)
}
x <- b5[(lula + flavio) > 30 & !is.na(tx_biografia)]
r5 <- rbindlist(lapply(split(x, by = "NR_TURNO"), function(d) {
  X <- data.frame(tx_biografia_pp = 100 * d$tx_biografia)
  if ("p_bio" %in% names(d) && any(!is.na(d$p_bio))) X$biometria_cadastrada_pp <- 100 * d$p_bio
  fe_lm(logit_emp(d$lula, d$lula + d$flavio), X, fe = d$mun, cl = d$mun, w = d$lula + d$flavio)[, NR_TURNO := d$NR_TURNO[1]]
}))
message("  Taxa de habilitação biográfica x % Lula (logit), dentro do município:")
print(r5)
dec <- x[, .(decil = cut(rank(tx_biografia, ties.method = "first") / .N, seq(0, 1, 0.1), labels = 1:10),
             tx_biografia, s = lula / (lula + flavio), s_mun = sum(lula) / sum(lula + flavio)), by = .(NR_TURNO, mun)]
dec <- dec[, .(secoes = .N, tx_biografia_media_pct = round(100 * mean(tx_biografia), 2),
               lula_menos_municipio_pp = round(100 * mean(s - s_mun), 3)), by = .(NR_TURNO, decil)][order(NR_TURNO, decil)]
salvar(res5, "N5_biometria_resumo.csv"); salvar(r5, "N5_biometria_x_voto.csv"); salvar(dec, "N5_biometria_decis.csv")
salvar(b5[z_bio > z_limite, c(chave, "comparecimento_bu", "habilitados_biometria", "habilitados_biografia",
                              "tx_biografia", "z_bio", "lula", "flavio"), with = FALSE][order(-z_bio)],
       "N5_secoes_sinalizadas.csv")

# =============================================================================
# Conferência do modelo da urna (assinatura x detalhe do TSE)
# =============================================================================
det <- ler_detalhe(extras = "DS_MODELO_URNA")
if (!is.null(det) && "DS_MODELO_URNA" %in% names(det)) {
  m <- merge(secs[!is.na(modelo) & modelo != "", c(chave, "modelo"), with = FALSE], det, by = chave)
  m[, igual := gsub("\\s", "", toupper(DS_MODELO_URNA)) == toupper(modelo)]
  message(sprintf("  Modelo da urna: assinatura = detalhe do TSE em %d de %d seções", sum(m$igual), nrow(m)))
  salvar(m[igual == FALSE], "N6_modelo_assinatura_x_detalhe_divergentes.csv")
}

message("\nConcluído.")

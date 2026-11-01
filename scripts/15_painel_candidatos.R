# =============================================================================
# 15_painel_candidatos.R - Votos de TODOS os candidatos por seção, para o painel
#
# Complementa o 14_painel_dados.R (que só guarda os dois finalistas). Grava em
# painel/dados/:
#   candidatos.json   - candidatos de cada turno (número, nome, votos) e totais
#                       por UF e Brasil, incluindo brancos e nulos
#   cand_uf_XX.json   - para cada seção da UF: votos de cada candidato nos dois
#                       turnos, aptos e comparecimento (detalhe da apuração)
#   municipios.json   - por município: população do Censo 2022 (IBGE), razão
#                       eleitorado / população de 15+ anos e o teste N12
#
# Só precisa do CSV de votação (01_baixar_dados.R). O detalhe (aptos e
# comparecimento) e as tabelas do N15/N12 são usados se existirem.
# Exterior (ZZ) fica de fora, como no 14.
# =============================================================================
source("00_config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
pasta_painel <- file.path("..", "painel", "dados")
dir.create(pasta_painel, recursive = TRUE, showWarnings = FALSE)
achar <- function(f) {
  for (d in c(pasta_saida, file.path("..", "resultados"), pasta_painel))
    if (file.exists(file.path(d, f))) return(file.path(d, f))
  NA_character_
}

# ---- Votação ------------------------------------------------------------------
etapa("Votação (todos os candidatos)")
pres <- ler_presidente(colunas = c(sec, "NM_MUNICIPIO", "NR_TURNO", "DS_CARGO",
                                   "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS"))
pres <- pres[SG_UF != "ZZ"]
for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(pres, j = cc, value = as.integer(pres[[cc]]))

# Candidatos de cada turno: mais votados primeiro; depois branco (95), nulo (96)
# e anulado (97), se houver
cand <- pres[, .(votos = sum(as.numeric(QT_VOTOS)), nome = NM_VOTAVEL[1]), by = .(NR_TURNO, NR_VOTAVEL)]
cand[, especial := NR_VOTAVEL %in% especiais]
setorder(cand, NR_TURNO, especial, -votos)
cand[especial == TRUE, nome := c(`95` = "Brancos", `96` = "Nulos", `97` = "Anulados")[as.character(NR_VOTAVEL)]]
print(cand[, .(NR_TURNO, NR_VOTAVEL, nome, votos)])

# Uma tabela larga por turno: uma linha por seção, uma coluna por candidato
largo <- function(t) {
  x <- dcast(pres[NR_TURNO == t], SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ NR_VOTAVEL,
             value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0L)
  cods <- as.character(cand[NR_TURNO == t, NR_VOTAVEL])
  for (cc in setdiff(cods, names(x))) x[, (cc) := 0L]     # candidato sem voto na base
  setnames(x, cods, paste0("t", t, "_", cods))
  x
}
turnos <- sort(unique(cand$NR_TURNO))
d <- Reduce(function(a, b) merge(a, b, by = sec, all = TRUE), lapply(turnos, largo))
nomes_mun <- unique(pres[, .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)], by = c("SG_UF", "CD_MUNICIPIO"))
rm(pres); gc(verbose = FALSE)

# Aptos e comparecimento (detalhe da apuração), se disponível
det <- ler_detalhe()
if (!is.null(det)) {
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(det, j = cc, value = as.integer(det[[cc]]))
  ap <- dcast(det[SG_UF != "ZZ"], SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ NR_TURNO,
              value.var = c("QT_APTOS", "QT_COMPARECIMENTO"))
  setnames(ap, sub("^QT_APTOS_", "apt_", sub("^QT_COMPARECIMENTO_", "cmp_", names(ap))))
  d <- merge(d, ap, by = sec, all.x = TRUE)
  rm(det, ap)
}
setorder(d, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)
message("  Seções (sem exterior): ", nrow(d))

# Conferência: soma das colunas = votos de cada candidato
for (t in turnos) {
  cods <- cand[NR_TURNO == t, NR_VOTAVEL]
  soma <- sapply(paste0("t", t, "_", cods), function(cc) sum(as.numeric(d[[cc]]), na.rm = TRUE))
  if (any(abs(soma - cand[NR_TURNO == t, votos]) > 0)) stop("Conferência falhou no ", t, "º turno")
}
message("  Conferência OK: totais por candidato batem com o arquivo do TSE")

# ---- candidatos.json --------------------------------------------------------------
etapa("candidatos.json")
tot <- function(x) {
  out <- lapply(turnos, function(t) {
    cods <- cand[NR_TURNO == t, NR_VOTAVEL]
    unname(sapply(paste0("t", t, "_", cods), function(cc) sum(as.numeric(x[[cc]]), na.rm = TRUE)))
  })
  names(out) <- as.character(turnos)
  for (t in turnos) for (v in c("apt_", "cmp_")) {
    cc <- paste0(v, t)
    if (cc %in% names(x)) out[[cc]] <- sum(as.numeric(x[[cc]]), na.rm = TRUE)
  }
  out
}
ufs <- sort(unique(d$SG_UF))
cands_json <- lapply(turnos, function(t) cand[NR_TURNO == t, .(num = NR_VOTAVEL, nome, votos, especial)])
names(cands_json) <- as.character(turnos)
por_uf <- lapply(ufs, function(u) tot(d[SG_UF == u])); names(por_uf) <- ufs
jsonlite::write_json(list(gerado_em = format(Sys.time(), "%d/%m/%Y %H:%M"), candidatos = cands_json,
                          brasil = tot(d), ufs = por_uf),
                     file.path(pasta_painel, "candidatos.json"), auto_unbox = TRUE, digits = NA)

# ---- cand_uf_XX.json ---------------------------------------------------------------
etapa("Arquivos por UF")
for (u in ufs) {
  x <- d[SG_UF == u]
  saida <- list(uf = u, mun = x$CD_MUNICIPIO, zona = x$NR_ZONA, secao = x$NR_SECAO)
  for (t in turnos) {                       # votos[[turno]][[candidato]] = vetor por seção
    cods <- cand[NR_TURNO == t, NR_VOTAVEL]
    saida[[paste0("v", t)]] <- unname(lapply(paste0("t", t, "_", cods), function(cc) x[[cc]]))
  }
  for (cc in grep("^(apt|cmp)_", names(x), value = TRUE)) saida[[sub("_", "", cc)]] <- x[[cc]]
  arq <- file.path(pasta_painel, sprintf("cand_uf_%s.json", tolower(u)))
  jsonlite::write_json(saida, arq, auto_unbox = TRUE, na = "null", digits = NA)
  message(sprintf("  %s: %d seções, %.1f MB", u, nrow(x), file.size(arq) / 1e6))
}

# ---- municipios.json (IBGE e N12) -------------------------------------------------
etapa("municipios.json")
mun <- copy(nomes_mun)
f <- achar("N15_eleitorado_x_populacao_municipios.csv")
if (!is.na(f)) {
  e <- fread(f)[, .(SG_UF, CD_MUNICIPIO = as.integer(CD_MUNICIPIO), cod_ibge = CD_MUN, nome_ibge = NM_MUNICIPIO_IBGE,
                    pop, pop15, razao = round(razao_aptos_pop15, 3))]
  mun <- merge(mun, e, by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)
} else message("  N15_eleitorado_x_populacao_municipios.csv não encontrado (rode o 13): sem população do IBGE")
f <- achar("N12_municipios_multimodais.csv")
if (!is.na(f)) {
  m12 <- fread(f)[, .(SG_UF, CD_MUNICIPIO = as.integer(CD_MUNICIPIO), n12_dip = round(dip, 4),
                      n12_q = signif(q, 3), n12_2022 = as.logical(mm22))]
  m12 <- unique(m12, by = c("SG_UF", "CD_MUNICIPIO"))
  mun <- merge(mun, m12, by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)
}
mun <- mun[SG_UF %in% ufs]
setorder(mun, SG_UF, CD_MUNICIPIO)
mj <- lapply(ufs, function(u) { x <- mun[SG_UF == u, !"SG_UF"]; as.list(x) }); names(mj) <- ufs
jsonlite::write_json(mj, file.path(pasta_painel, "municipios.json"), auto_unbox = FALSE, na = "null", digits = NA)
message("  ", nrow(mun), " municípios")

message("\nConcluído. Arquivos em ", normalizePath(pasta_painel))

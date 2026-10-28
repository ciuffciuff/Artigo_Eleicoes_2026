# Simulador de dados no layout do TSE para testar os scripts 07, 08 e 10
# Uso: Rscript simular.R <pasta_destino>
suppressPackageStartupMessages(library(data.table))
set.seed(1)
args <- commandArgs(TRUE); dest <- if (length(args)) args[1] else "simdata"
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR",
         "RJ","RN","RO","RR","RS","SC","SE","SP","TO","ZZ")
latin1 <- function(dt) { for (cc in names(dt)[sapply(dt, is.character)]) set(dt, j = cc, value = iconv(dt[[cc]], "UTF-8", "latin1")); dt }
w <- function(dt, f) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); fwrite(latin1(copy(dt)), f, sep = ";") }

# estrutura: UF > município > zona > local > seção
estr <- rbindlist(lapply(seq_along(ufs), function(i) {
  rbindlist(lapply(1:3, function(m) {
    nloc <- 12
    rbindlist(lapply(1:nloc, function(l) {
      ns <- sample(3:8, 1)
      data.table(SG_UF = ufs[i], CD_MUNICIPIO = i * 1000L + m, NM_MUNICIPIO = sprintf("MUNICÍPIO %s %d", ufs[i], m),
                 NR_ZONA = 1L + (l > 6), NR_LOCAL_VOTACAO = 1000L + l, NM_LOCAL_VOTACAO = sprintf("ESCOLA %d", l),
                 lat = -15 + i * 0.3 + m * 0.05 + l * 0.003, lon = -50 + i * 0.2 + l * 0.002, n_sec = ns)
    }))
  }))
}))
estr <- estr[rep(seq_len(.N), n_sec)][, NR_SECAO := seq_len(.N), by = .(SG_UF, CD_MUNICIPIO, NR_ZONA)][, n_sec := NULL]
N <- nrow(estr)
estr[, aptos := sample(150:420, N, TRUE)]
estr[, ef_mun := rnorm(1, 0, 0.6), by = .(SG_UF, CD_MUNICIPIO)]
estr[, ef_loc := rnorm(1, 0, 0.3), by = .(SG_UF, CD_MUNICIPIO, NR_LOCAL_VOTACAO)]
# perfil
estr[, `:=`(p_fem = rbeta(N, 52, 48), p_sup = rbeta(N, 2 + 3 * (ef_loc < 0), 8), p_jov = rbeta(N, 2, 8), p_ido = rbeta(N, 2, 7))]
estr[, lp := ef_mun + ef_loc - 2 * (p_sup - 0.2) + rnorm(N, 0, 0.15)]
estr[, modelo := sample(c("UE2015", "UE2020", "UE2022", "UE2013"), N, TRUE, prob = c(.2, .4, .3, .1))]
votos <- list(); det <- list()
for (t in 1:2) {
  comp <- rbinom(N, estr$aptos, plogis(1.3 + rnorm(N, 0, 0.2) - 0.05 * (t == 2)))
  if (t == 1) {
    brnul <- rbinom(N, comp, 0.05); val <- comp - brnul
    pr <- cbind(plogis(estr$lp) * 0.92, (1 - plogis(estr$lp)) * 0.92, 0.05, 0.03)
    cnt <- t(sapply(seq_len(N), function(i) rmultinom(1, val[i], pr[i, ])))
    cand <- data.table(`13` = cnt[, 1], `22` = cnt[, 2], `30` = cnt[, 3], `55` = cnt[, 4])
  } else {
    brnul <- rbinom(N, comp, 0.04); val <- comp - brnul
    l <- rbinom(N, val, plogis(estr$lp + 0.08))
    cand <- data.table(`13` = l, `22` = val - l)
  }
  nb <- rbinom(N, brnul, 0.4)
  cand[, `95` := nb][, `96` := brnul - nb]
  # anomalias plantadas no 2º turno: 5 seções com enchimento para o 13
  if (t == 2) { ii <- sample(N, 5); add <- floor(0.8 * (estr$aptos[ii] - comp[ii])); cand$`13`[ii] <- cand$`13`[ii] + add; comp[ii] <- comp[ii] + add
    message("enchimento plantado: ", paste(estr[ii, paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)], collapse = " | ")) }
  lng <- melt(cbind(estr[, .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO)], cand),
              measure.vars = names(cand), variable.name = "NR_VOTAVEL", value.name = "QT_VOTOS")
  lng[, `:=`(NR_TURNO = t, DS_CARGO = "Presidente", NR_VOTAVEL = as.integer(as.character(NR_VOTAVEL)))]
  lng[, NM_VOTAVEL := c(`13` = "LUIZ INÁCIO LULA DA SILVA", `22` = "FLAVIO NANTES BOLSONARO", `30` = "ROMEU ZEMA NETO",
                        `55` = "RONALDO RAMOS CAIADO", `95` = "VOTO BRANCO", `96` = "VOTO NULO")[as.character(NR_VOTAVEL)]]
  votos[[t]] <- lng[QT_VOTOS > 0]
  det[[t]] <- estr[, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_TURNO = t, DS_CARGO = "Presidente", QT_APTOS = aptos,
                       QT_COMPARECIMENTO = comp, DS_MODELO_URNA = sub("UE", "UE ", modelo), DS_ORIGEM_VOTO = "Urna Eletrônica")]
}
w(rbindlist(votos), file.path(dest, "tse_2026/csv/votacao_secao_2026_BR.csv"))
w(rbindlist(det), file.path(dest, "tse_2026/detalhe/detalhe_votacao_secao_2026_BR.csv"))

# perfil por UF
gen <- c("FEMININO", "MASCULINO"); fx <- c("16 anos", "21 a 24 anos", "35 a 39 anos", "50 a 54 anos", "65 a 69 anos")
esc <- c("ANALFABETO", "LÊ E ESCREVE", "ENSINO MÉDIO COMPLETO", "SUPERIOR COMPLETO")
for (uf in ufs) {
  e <- estr[SG_UF == uf]
  p <- e[, {
    pg <- c(p_fem, 1 - p_fem); pf <- pmax(c(p_jov / 2, p_jov / 2, (1 - p_jov - p_ido) / 2, (1 - p_jov - p_ido) / 2, p_ido), 0.01)
    pe <- c(0.1, 0.1, 0.8 - p_sup, p_sup)
    g <- CJ(DS_GENERO = gen, DS_FAIXA_ETARIA = fx, DS_GRAU_ESCOLARIDADE = esc, sorted = FALSE)
    pr <- pg[match(g$DS_GENERO, gen)] * pf[match(g$DS_FAIXA_ETARIA, fx)] * pe[match(g$DS_GRAU_ESCOLARIDADE, esc)]
    q <- as.vector(rmultinom(1, aptos, pr))
    g[, `:=`(QT_ELEITORES = q, QT_ELEITORES_BIOMETRIA = rbinom(length(q), q, 0.85))][QT_ELEITORES > 0]
  }, by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO)]
  w(p, file.path(dest, sprintf("tse_2026/extra/perfil/perfil_eleitor_secao_2026_%s.csv", uf)))
  w(e[, .(NR_TURNO = 2L, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO,
          NR_LATITUDE = sub(".", ",", sprintf("%.6f", lat), fixed = TRUE),
          NR_LONGITUDE = sub(".", ",", sprintf("%.6f", lon), fixed = TRUE))],
    file.path(dest, sprintf("tse_2026/extra/locais/eleitorado_local_votacao_2026_%s.csv", uf)))
}
# 2022: mesmo eleitorado, voto correlacionado
v22 <- estr[, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, n = rbinom(N, aptos, 0.78))]
v22[, l := rbinom(N, n, plogis(estr$lp - 0.1 + rnorm(N, 0, 0.1)))]
v22 <- rbind(v22[, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, NR_TURNO = 2L, DS_CARGO = "Presidente", NR_VOTAVEL = 13L, QT_VOTOS = l)],
             v22[, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, NR_TURNO = 2L, DS_CARGO = "Presidente", NR_VOTAVEL = 22L, QT_VOTOS = n - l)])
w(v22, file.path(dest, "tse_2026/extra/votacao2022/votacao_secao_2022_BR.csv"))

# saídas do 09 (arquivos de urna) para AC, RR e DF, 1º e 2º turno
vv <- rbindlist(votos); dd <- rbindlist(det)
for (t in 1:2) for (uf in c("AC", "RR", "DF")) {
  d <- dd[SG_UF == uf & NR_TURNO == t]
  s <- d[, .(turno = t, uf = tolower(SG_UF), cd_mun = sprintf("%05d", CD_MUNICIPIO), zona = sprintf("%04d", NR_ZONA),
             secao = sprintf("%04d", NR_SECAO), status = "ok", origem_bu = "bu", modelo = sub(" ", "", DS_MODELO_URNA),
             aptos_bu = QT_APTOS, comparecimento_bu = QT_COMPARECIMENTO,
             habilitados_biometria = rbinom(.N, QT_COMPARECIMENTO, 0.9))]
  s[, habilitados_biografia := rbinom(.N, comparecimento_bu - habilitados_biometria, 0.6)]
  s[, `:=`(rdv_confere = "True", hash_bu_ok = "True", hash_rdv_ok = "True", hash_log_ok = "True",
           assinatura_valida = "True", assinatura_algoritmo = "Ed521", origem_log = "log",
           votos_log = comparecimento_bu, diferenca_log_bu = 0, votos_apos_17h = rbinom(.N, comparecimento_bu, 0.01),
           votos_antes_8h = 0, votos_antes_pronta = 0, votos_apos_encerramento = 0, max_votos_minuto = sample(2:4, .N, TRUE),
           intervalos_lt_10s = rbinom(.N, 20, 0.1), intervalos_lt_5s = 0, fuso_offset = c(AC = -2, RR = -1, DF = 0)[toupper(uf)])]
  if (uf == "DF" && t == 2) { s$rdv_confere[1] <- "False"; s$hash_bu_ok[2] <- "False"; s$max_votos_minuto[3] <- 40 }
  dir.create(file.path(dest, "tse_2026/urna/saida"), recursive = TRUE, showWarnings = FALSE)
  fwrite(s, file.path(dest, sprintf("tse_2026/urna/saida/secoes_t%d_%s.csv", t, uf)), sep = ";")
  vo <- vv[SG_UF == uf & NR_TURNO == t, .(turno = t, uf = tolower(SG_UF), cd_mun = sprintf("%05d", CD_MUNICIPIO),
           zona = sprintf("%04d", NR_ZONA), secao = sprintf("%04d", NR_SECAO), fonte = "bu",
           tipo = fifelse(NR_VOTAVEL == 95, "branco", fifelse(NR_VOTAVEL == 96, "nulo", "nominal")),
           numero = fifelse(NR_VOTAVEL >= 95, NA_integer_, NR_VOTAVEL), votos = QT_VOTOS)]
  if (uf == "DF" && t == 2) vo$votos[1] <- vo$votos[1] + 7L
  fwrite(vo, file.path(dest, sprintf("tse_2026/urna/saida/votos_t%d_%s.csv", t, uf)), sep = ";")
  h <- s[, .(hora_local = sprintf("%02d", 8:17 + fuso_offset), votos = as.vector(rmultinom(1, comparecimento_bu, c(5, 12, 12, 11, 10, 10, 11, 12, 12, 5)))),
         by = .(turno, uf, cd_mun, zona, secao)]
  fwrite(h, file.path(dest, sprintf("tse_2026/urna/saida/horas_t%d_%s.csv", t, uf)), sep = ";")
}
message("Simulação gravada em ", dest, ": ", N, " seções")

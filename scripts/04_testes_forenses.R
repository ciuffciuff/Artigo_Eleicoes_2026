# =============================================================================
# 04_testes_forenses.R - Testes estatísticos de padrão de votos (T1 a T7)
# Presidente - Eleições 2026
# Todas as seções apuradas, 1º e 2º turno
#
# Pré-requisito: 01 e 02 rodados com sucesso.
#
# IMPORTANTE - como interpretar:
#   * Estes testes apontam PADRÕES INCOMUNS. Padrão incomum NÃO é prova de
#     fraude: há muitas explicações legítimas (perfil dos eleitores de cada
#     seção, seções em presídios/aldeias/exterior, voto estratégico etc.).
#   * Com ~480 mil seções, qualquer desvio minúsculo vira "p < 0,001".
#     Olhe sempre o TAMANHO do desvio (colunas de efeito), não só o p-valor.
#   * Testes por seção usam correção para múltiplas comparações (BH/FDR).
#
# Testes incluídos:
#   T1. Zeros e 100%: seções com 0 votos ou 100% dos válidos para um
#       candidato, comparadas com o esperado pelas outras seções do local.
#   T2. Último dígito (Beber & Scacco, 2012): uniforme (0-9) em teoria.
#   T3. Segundo dígito / 2BL (Mebane): Benford do 2º dígito em teoria.
#       T2 e T3 também são comparados com uma referência simulada, que é
#       a mais confiável (ver comentário na seção T2/T3).
#   T4. Percentuais "redondos" (Kobak et al., 2016): excesso de seções com
#       percentual inteiro exato (ex.: 50,0%, 70,0%), via simulação.
#   T5. Homogeneidade no local de votação: cada seção comparada com as
#       outras seções do mesmo local (mesma escola/prédio).
#   T6. Consistência 1º x 2º turno: seções cujo resultado no 2º turno foge
#       muito do previsto pelo 1º turno da própria seção.
#   T7. (Opcional) Comparecimento x voto - "impressão digital eleitoral"
#       (Klimek et al., 2012). Precisa do arquivo "Detalhe da votação por
#       seção", que traz eleitores aptos.
#
# Testes deixados de fora de propósito:
#   * Benford do 1º dígito: não vale para votos por seção (seções têm
#     tamanho limitado, ~300-400 eleitores), então "falha" sempre.
#   * Testes de normalidade (Shapiro etc.): com centenas de milhares de
#     seções rejeitam sempre e não dizem nada sobre fraude.
#   * Modelo de urna, horários, log: não estão nestes CSVs (exigem os
#     arquivos de urna - BU/log).
# =============================================================================

source("00_config.R")
set.seed(2026)                           # simulações reprodutíveis

# ---- Parâmetros dos testes ------------------------------------------------------
alfa_fdr    <- 0.001   # q-valor (FDR) máximo para sinalizar uma seção
dif_minima  <- 0.20    # diferença mínima (20 p.p.) para sinalizar em T5
z_robusto   <- 6       # |z| robusto mínimo para sinalizar em T6
n_simul     <- 100     # nº de simulações em T4 (mais = mais preciso e lento)
min_votos_digito <- 10 # T2/T3 usam só contagens com 2+ dígitos

# ---- 1. Leitura e teste de carga --------------------------------------------------
etapa("Leitura e teste de carga")
pres <- ler_presidente()
pres[, valido := !(NR_VOTAVEL %in% especiais)]
turnos <- sort(unique(pres$NR_TURNO))
nomes  <- unique(pres[, .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)])

# ---- 2. Bases de trabalho ----------------------------------------------------
# 2a. Uma linha por seção
secoes <- pres[, .(total            = sum(QT_VOTOS),
                   validos          = sum(QT_VOTOS[valido]),
                   NR_LOCAL_VOTACAO = NR_LOCAL_VOTACAO[1],
                   NM_LOCAL_VOTACAO = NM_LOCAL_VOTACAO[1]),
               by = chave]

# 2b. Uma linha por seção x candidato, INCLUINDO candidatos com 0 votos
#     (o arquivo do TSE normalmente não traz linhas com 0 votos)
cand_validos <- unique(pres[valido == TRUE, .(NR_TURNO, NR_VOTAVEL)])
grade <- merge(secoes, cand_validos, by = "NR_TURNO", allow.cartesian = TRUE)
votos_cand <- pres[valido == TRUE, .(votos = sum(QT_VOTOS)),
                   by = c(chave, "NR_VOTAVEL")]
cv <- merge(grade, votos_cand, by = c(chave, "NR_VOTAVEL"), all.x = TRUE)
cv[is.na(votos), votos := 0L]
cv <- merge(cv, nomes, by = c("NR_TURNO", "NR_VOTAVEL"), all.x = TRUE)
cv[, share := fifelse(validos > 0, votos / validos, NA_real_)]
rm(grade, votos_cand); invisible(gc())

# Participação do candidato no município (referência para T1)
cv[, p_mun := sum(votos) / sum(validos),
   by = .(NR_TURNO, SG_UF, CD_MUNICIPIO, NR_VOTAVEL)]

message("Seções por turno:"); print(secoes[, .N, by = NR_TURNO])

# =============================================================================
# T1. Zeros e 100%
# Esperado: soma, sobre as seções, da probabilidade binomial de 0 votos
# (ou de todos os votos), usando como referência o % do candidato nas OUTRAS
# seções do mesmo local de votação (se o local só tem 1 seção, o município).
# p-valor: aproximação de Poisson (obs >= esperado).
# =============================================================================
message("\nT1 - zeros e 100% ...")
loc <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_LOCAL_VOTACAO",
         "NR_VOTAVEL")
cv[, `:=`(votos_local    = sum(votos),
          validos_local  = sum(validos),
          n_secoes_local = .N), by = loc]
cv[, p_ref := fifelse(validos_local - validos > 0,
                      (votos_local - votos) / (validos_local - validos),
                      p_mun)]

t1 <- cv[validos > 0, .(
  secoes    = .N,
  zeros_obs = sum(votos == 0),
  zeros_esp = round(sum(dbinom(0, validos, p_ref)), 3),
  cem_obs   = sum(votos == validos),
  cem_esp   = round(sum(dbinom(validos, validos, p_ref)), 3)
), by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)]
t1[, `:=`(zeros_p = ppois(zeros_obs - 1, zeros_esp, lower.tail = FALSE),
          cem_p   = ppois(cem_obs   - 1, cem_esp,   lower.tail = FALSE))]
setorder(t1, NR_TURNO, NR_VOTAVEL)
print(t1)
salvar(t1, "T1_zeros_e_100.csv")

# Lista das seções com 0% ou 100% quando o candidato tem entre 5% e 95%
# nas outras seções do local (para conferência manual)
salvar(cv[validos > 0 & (votos == 0 | votos == validos) & p_ref > 0.05 & p_ref < 0.95,
          c(chave, "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "NM_VOTAVEL", "votos",
            "validos", "p_ref"),
          with = FALSE],
       "T1_secoes_0_ou_100.csv")

# =============================================================================
# T2 e T3. Testes de dígitos
# =============================================================================
message("\nT2/T3 - dígitos ...")

benford2 <- sapply(0:9, function(d) sum(log10(1 + 1 / (10 * (1:9) + d))))

ult_dig <- function(x) x %% 10L
seg_dig <- function(x) as.integer(substr(as.character(x), 2, 2))

# Compara frequências observadas com: (a) a referência teórica (uniforme no
# último dígito; Benford no 2º) e (b) uma referência SIMULADA - votos sorteados
# (binomial) com o mesmo tamanho de seção e o mesmo % observado. A (b) é a
# referência mais justa: candidatos com poucos votos por seção "falham" na
# (a) mesmo sem nada errado, porque contagens pequenas têm dígitos enviesados.
# Só roda quando o candidato tem, em média, votos suficientes por seção
# (mediana >= 50). Com poucos votos por seção os testes de dígito não são
# confiáveis nem com a referência simulada.
teste_digitos <- function(x, n = NULL, p = NULL, n_sim = 20, segundo = TRUE) {
  if (median(x) < 50) return(NULL)
  usar <- x >= min_votos_digito
  if (sum(usar) < 100) return(NULL)
  res <- data.table(n_contagens = sum(usar))
  for (tipo in if (segundo) c("ult", "seg") else "ult") {
    f    <- if (tipo == "ult") ult_dig else seg_dig
    teor <- if (tipo == "ult") rep(0.1, 10) else benford2
    obs  <- tabulate(f(x[usar]) + 1L, nbins = 10)
    qt   <- suppressWarnings(chisq.test(obs, p = teor))
    res[, paste0(tipo, "_media")      := round(sum((0:9) * obs) / sum(obs), 3)]
    res[, paste0(tipo, "_p_teorico")  := qt$p.value]
    if (!is.null(n)) {
      sim <- unlist(lapply(seq_len(n_sim), function(i) rbinom(length(n), n, p)))
      sim <- sim[sim >= min_votos_digito]
      esp <- tabulate(f(sim) + 1L, nbins = 10) / length(sim)
      esp <- pmax(esp, 1e-12)
      qs  <- suppressWarnings(chisq.test(obs, p = esp / sum(esp)))
      res[, paste0(tipo, "_p_simulado") := qs$p.value]
      res[, paste0(tipo, "_desvio_max_pp") :=
            round(100 * max(abs(obs / sum(obs) - esp)), 2)]
    }
  }
  res
}

t23 <- rbind(
  cv[validos > 0, teste_digitos(votos, validos, share),
     by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)],
  # total da seção: só último dígito (o 2º dígito de totais entre ~100 e ~400
  # não segue Benford por construção)
  secoes[, teste_digitos(total, segundo = FALSE), by = NR_TURNO][, `:=`(NR_VOTAVEL = NA_integer_,
                                                       NM_VOTAVEL = "TOTAL DA SEÇÃO")],
  fill = TRUE
)
setorder(t23, NR_TURNO, NR_VOTAVEL)
print(t23)
salvar(t23, "T2_T3_digitos.csv")

# Gráfico: distribuição do último dígito, por candidato, em cada turno
for (tr in turnos) {
  # só candidatos com votos suficientes (mesma regra da tabela)
  ok <- cv[NR_TURNO == tr, .(m = median(votos)), by = NR_VOTAVEL][m >= 50, NR_VOTAVEL]
  dd <- cv[NR_TURNO == tr & NR_VOTAVEL %in% ok & votos >= min_votos_digito,
           .(n = .N), by = .(NM_VOTAVEL, dig = votos %% 10)]
  dd[, prop := n / sum(n), by = NM_VOTAVEL]
  cands <- unique(dd$NM_VOTAVEL)
  png(file.path(pasta_saida, sprintf("T2_ultimo_digito_turno%d.png", tr)),
      width = 300 * min(length(cands), 4), height = 300 * ceiling(length(cands) / 4))
  par(mfrow = c(ceiling(length(cands) / 4), min(length(cands), 4)))
  for (cd in cands) {
    v <- dd[NM_VOTAVEL == cd][order(dig)]
    barplot(v$prop, names.arg = v$dig, main = substr(cd, 1, 25),
            ylim = c(0, 0.2), col = "grey70", border = NA)
    abline(h = 0.1, col = "red", lty = 2)
  }
  dev.off()
}

# =============================================================================
# T4. Percentuais redondos (Kobak et al., 2016)
# Conta seções em que o candidato tem percentual inteiro exato dos válidos
# (excluindo 0% e 100%, já vistos em T1) e compara com simulações binomiais.
# =============================================================================
message("\nT4 - percentuais redondos (", n_simul, " simulações por candidato) ...")

eh_inteiro <- function(v, n) v > 0 & v < n & (100L * v) %% n == 0L

t4 <- cv[validos > 0, {
  obs <- sum(eh_inteiro(votos, validos))
  sim <- vapply(seq_len(n_simul), function(i) {
    k <- rbinom(.N, validos, share)
    sum(eh_inteiro(k, validos))
  }, numeric(1))
  .(secoes = .N,
    inteiro_obs  = obs,
    inteiro_esp  = round(mean(sim), 1),
    razao        = round(obs / max(mean(sim), 1e-9), 3),
    p_empirico   = (sum(sim >= obs) + 1) / (n_simul + 1))
}, by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)]
setorder(t4, NR_TURNO, NR_VOTAVEL)
print(t4)
salvar(t4, "T4_percentuais_redondos.csv")

# Gráfico: histograma fino do % de válidos (picos em números redondos
# aparecem como "espinhos")
for (tr in turnos) {
  cands <- unique(cv[NR_TURNO == tr, NM_VOTAVEL])
  png(file.path(pasta_saida, sprintf("T4_histograma_pct_turno%d.png", tr)),
      width = 350 * min(length(cands), 4), height = 300 * ceiling(length(cands) / 4))
  par(mfrow = c(ceiling(length(cands) / 4), min(length(cands), 4)))
  for (cd in cands) {
    x <- cv[NR_TURNO == tr & NM_VOTAVEL == cd & validos > 0, 100 * share]
    hist(x, breaks = seq(0, 100, by = 0.1), main = substr(cd, 1, 25),
         xlab = "% dos válidos na seção", ylab = "Nº de seções",
         col = "grey40", border = NA)
  }
  dev.off()
}

# =============================================================================
# T5. Homogeneidade no local de votação
# Cada seção x as demais seções do mesmo local (mesmo prédio).
# Teste de duas proporções; correção BH para múltiplas comparações.
# ATENÇÃO: seções de um mesmo local NÃO têm eleitores sorteados (ex.: seções
# mais novas costumam ter eleitores mais jovens). Diferenças moderadas são
# esperadas; por isso o z é ajustado pela dispersão e só sinalizamos
# diferença >= dif_minima. Se uma seção destoa muito, as outras do mesmo
# local também podem aparecer (porque a referência delas inclui a destoante).
# =============================================================================
message("\nT5 - homogeneidade no local de votação ...")

t5 <- cv[n_secoes_local >= 2 & validos >= 30 & (validos_local - validos) >= 30 &
         votos_local > 0 & votos_local < validos_local]
t5[, `:=`(p_secao  = votos / validos,
          p_outras = (votos_local - votos) / (validos_local - validos),
          p_pool   = votos_local / validos_local)]
t5[, z := (p_secao - p_outras) /
          sqrt(p_pool * (1 - p_pool) * (1 / validos + 1 / (validos_local - validos)))]
t5[!is.finite(z), z := 0]

# Dispersão: variância observada / esperada se os eleitores fossem sorteados
# entre as seções do local (binomial). 1 = tão parecidas quanto um sorteio;
# > 1 = mais diferentes (normal, porque eleitores não são sorteados).
t5[, razao_dispersao := max(1, sum((votos - validos * p_pool)^2 /
                                   (validos * p_pool * (1 - p_pool))) / .N),
   by = .(NR_TURNO, NR_VOTAVEL)]
# z ajustado pela dispersão (quase-binomial): evita sinalizar seções só
# porque todas as seções do país variam mais que um sorteio
t5[, z_ajustado := z / sqrt(razao_dispersao)]
t5[, p_valor := 2 * pnorm(-abs(z_ajustado))]
t5[, q_valor := p.adjust(p_valor, method = "BH")]
t5[, diferenca_pp := round(100 * (p_secao - p_outras), 1)]

t5_disp <- t5[, .(
  secoes_testadas = .N,
  razao_dispersao = round(razao_dispersao[1], 2),
  sinalizadas     = sum(q_valor < alfa_fdr & abs(diferenca_pp) >= 100 * dif_minima)
), by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)]
setorder(t5_disp, NR_TURNO, NR_VOTAVEL)
print(t5_disp)
salvar(t5_disp, "T5_resumo_homogeneidade_local.csv")

t5_sinal <- t5[q_valor < alfa_fdr & abs(diferenca_pp) >= 100 * dif_minima,
               c(chave, "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "NM_VOTAVEL", "votos", "validos",
                 "n_secoes_local", "p_secao", "p_outras", "diferenca_pp",
                 "z_ajustado", "q_valor"), with = FALSE][order(q_valor)]
salvar(t5_sinal, "T5_secoes_sinalizadas.csv")
message("T5: ", nrow(t5_sinal), " combinações seção-candidato sinalizadas")
rm(t5); invisible(gc())

# =============================================================================
# T6. Consistência entre 1º e 2º turno (mesma seção)
# Modelo: % de cada finalista no 2º turno explicado pelos % de todos os
# candidatos (e brancos/nulos) no 1º turno, com efeito fixo de município.
# Sinaliza resíduos extremos (z robusto) e mudanças extremas de comparecimento.
# =============================================================================
if (all(c(1L, 2L) %in% turnos)) {
  message("\nT6 - consistência 1º x 2º turno ...")

  # % de cada votável (inclui branco/nulo) sobre o total, no 1º turno
  t1_largo <- dcast(pres[NR_TURNO == 1, .(votos = sum(QT_VOTOS)),
                         by = c(sec, "NR_VOTAVEL")],
                    as.formula(paste(paste(sec, collapse = " + "), "~ NR_VOTAVEL")),
                    value.var = "votos", fill = 0)
  cols1 <- setdiff(names(t1_largo), sec)
  t1_largo[, total1 := rowSums(.SD), .SDcols = cols1]
  t1_largo <- t1_largo[total1 > 0]
  for (cc in cols1) set(t1_largo, j = cc, value = t1_largo[[cc]] / t1_largo$total1)
  setnames(t1_largo, cols1, paste0("t1_", cols1))
  cols1 <- paste0("t1_", cols1)
  # retira uma categoria para evitar colinearidade (as % somam 1)
  cols1_mod <- head(cols1, -1)

  s2 <- secoes[NR_TURNO == 2 & validos > 0, c(sec, "total", "validos"), with = FALSE]
  base6 <- merge(s2, t1_largo, by = sec)
  base6[, var_comparec := log(total / total1)]
  message("Seções presentes nos dois turnos: ", nrow(base6))

  finalistas <- unique(cv[NR_TURNO == 2, .(NR_VOTAVEL, NM_VOTAVEL)])

  # z robusto (mediana e MAD) - não depende de normalidade
  z_rob <- function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)

  # Efeito fixo de município = subtrair médias do município (ponderadas)
  centrar <- function(dt, cols, peso, grupo) {
    for (cc in cols) {
      m <- dt[, .(m = weighted.mean(get(cc), get(peso))), by = grupo]
      dt[m, on = grupo, (cc) := get(cc) - i.m]
    }
    dt
  }

  res6 <- list(); sinal6 <- list()
  for (i in seq_len(nrow(finalistas))) {
    nv <- finalistas$NR_VOTAVEL[i]; nm <- finalistas$NM_VOTAVEL[i]
    v2 <- cv[NR_TURNO == 2 & NR_VOTAVEL == nv, c(sec, "share"), with = FALSE]
    d  <- merge(base6, v2, by = sec)
    dc <- centrar(copy(d), c("share", cols1_mod), "validos",
                  c("SG_UF", "CD_MUNICIPIO"))
    f  <- as.formula(paste("share ~ 0 +", paste0("`", cols1_mod, "`", collapse = " + ")))
    mod <- lm(f, data = dc, weights = validos)
    d[, residuo := residuals(mod) * sqrt(validos)]   # resíduo ponderado
    d[, z := z_rob(residuo)]
    res6[[i]] <- data.table(NR_VOTAVEL = nv, NM_VOTAVEL = nm,
                            secoes = nrow(d),
                            r2_dentro_municipio = round(summary(mod)$r.squared, 4),
                            sinalizadas = sum(abs(d$z) >= z_robusto))
    sinal6[[i]] <- d[abs(z) >= z_robusto,
                     c(sec, "validos", "share", "z"), with = FALSE][
                       , NM_VOTAVEL := nm]
  }
  t6 <- rbindlist(res6)
  print(t6)
  salvar(t6, "T6_resumo_1o_x_2o_turno.csv")
  s6 <- rbindlist(sinal6)
  if (nrow(s6) > 0)
    s6 <- merge(s6, unique(secoes[NR_TURNO == 2, c(sec, "NM_LOCAL_VOTACAO"),
                                  with = FALSE]), by = sec, all.x = TRUE)
  salvar(s6[order(-abs(z))], "T6_secoes_sinalizadas.csv")

  # Mudança de comparecimento (nº de votos) entre os turnos
  # Compara cada seção com as outras do MESMO município. Sem isso, estados
  # inteiros em que o comparecimento caiu no 2º turno (ex.: sem disputa de
  # governador) apareceriam como "anomalias".
  base6[, var_comparec_mun := var_comparec - median(var_comparec),
        by = .(SG_UF, CD_MUNICIPIO)]
  base6[, n_secoes_mun := .N, by = .(SG_UF, CD_MUNICIPIO)]
  base6[, z_comparec := z_rob(var_comparec_mun)]
  base6[n_secoes_mun < 3, z_comparec := NA_real_]   # sem referência local
  base6 <- merge(base6, unique(secoes[NR_TURNO == 2, c(sec, "NM_LOCAL_VOTACAO"),
                                      with = FALSE]), by = sec, all.x = TRUE)
  t6c <- base6[!is.na(z_comparec) & abs(z_comparec) >= z_robusto,
               c(sec, "NM_LOCAL_VOTACAO", "total1", "total", "z_comparec"),
               with = FALSE]
  setnames(t6c, c("total1", "total"), c("votos_1turno", "votos_2turno"))
  salvar(t6c[order(-abs(z_comparec))], "T6_comparecimento_sinalizadas.csv")
  message("T6: ", nrow(t6c), " seções com mudança extrema de comparecimento")

  # Gráfico: % no 1º turno x % no 2º turno (primeiro finalista)
  nv1 <- finalistas$NR_VOTAVEL[1]
  col1 <- paste0("t1_", nv1)
  if (col1 %in% names(base6)) {
    g <- merge(base6, cv[NR_TURNO == 2 & NR_VOTAVEL == nv1, c(sec, "share"), with = FALSE],
               by = sec)
    png(file.path(pasta_saida, "T6_1o_x_2o_turno.png"), width = 700, height = 700)
    smoothScatter(100 * g[[col1]], 100 * g$share,
                  xlab = paste("% no 1º turno (total) -", finalistas$NM_VOTAVEL[1]),
                  ylab = "% dos válidos no 2º turno", nrpoints = 0)
    dev.off()
  }
} else {
  message("\nT6 pulado: é preciso ter o 1º e o 2º turno nos arquivos.")
}

# =============================================================================
# T7. (Opcional) Comparecimento x voto - "impressão digital eleitoral"
# Precisa do "Detalhe da votação por seção" (Dados Abertos do TSE), com os
# CSVs descompactados em tse_2026/detalhe. Se não existir, o teste é pulado.
# =============================================================================
det <- ler_detalhe()

if (!is.null(det)) {
  message("\nT7 - comparecimento x voto ...")
  {
    d7 <- merge(cv, det, by = chave)
    d7 <- d7[QT_APTOS > 0]
    d7[, comparec := QT_COMPARECIMENTO / QT_APTOS]

    # Correlação comparecimento x % do candidato DENTRO do município
    t7 <- d7[validos > 0, {
      cc <- comparec - ave(comparec, SG_UF, CD_MUNICIPIO)
      ss <- share    - ave(share,    SG_UF, CD_MUNICIPIO)
      ct <- suppressWarnings(cor.test(cc, ss))
      .(secoes = .N,
        cor_dentro_mun = round(ct$estimate, 4),
        p_valor = ct$p.value,
        secoes_comparec_95_e_voto_95 = sum(comparec >= 0.95 & share >= 0.95))
    }, by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)]
    print(t7)
    salvar(t7, "T7_comparecimento_x_voto.csv")

    # Percentual de comparecimento inteiro exato (Kobak) - por turno
    t7k <- unique(d7[, c(chave, "QT_APTOS", "QT_COMPARECIMENTO"), with = FALSE])[, {
      obs <- sum(eh_inteiro(QT_COMPARECIMENTO, QT_APTOS))
      p   <- QT_COMPARECIMENTO / QT_APTOS
      sim <- vapply(seq_len(n_simul), function(i)
        sum(eh_inteiro(rbinom(.N, QT_APTOS, p), QT_APTOS)), numeric(1))
      .(inteiro_obs = obs, inteiro_esp = round(mean(sim), 1),
        p_empirico = (sum(sim >= obs) + 1) / (n_simul + 1))
    }, by = NR_TURNO]
    print(t7k)
    salvar(t7k, "T7_comparecimento_redondo.csv")

    # Gráfico "impressão digital": comparecimento x % do vencedor no turno
    for (tr in unique(d7$NR_TURNO)) {
      lider <- d7[NR_TURNO == tr, .(v = sum(votos)), by = NR_VOTAVEL][which.max(v), NR_VOTAVEL]
      g <- d7[NR_TURNO == tr & NR_VOTAVEL == lider & validos > 0]
      h <- table(cut(100 * g$comparec, seq(0, 100, 1), include.lowest = TRUE),
                 cut(100 * g$share,    seq(0, 100, 1), include.lowest = TRUE))
      png(file.path(pasta_saida, sprintf("T7_impressao_digital_turno%d.png", tr)),
          width = 700, height = 700)
      image(1:100, 1:100, log1p(unclass(h)), col = hcl.colors(50, "viridis"),
            xlab = "Comparecimento (%)",
            ylab = paste("% dos válidos -", g$NM_VOTAVEL[1]),
            main = sprintf("Impressão digital eleitoral - %dº turno", tr))
      dev.off()
    }
  }
} else {
  message("\nT7 pulado: detalhe da apuração não disponível em ", pasta_detalhe,
          " (rode 01_baixar_dados.R)")
}

message("\nConcluído. Resultados em: ", normalizePath(pasta_saida), ". Próximo passo: 05_cruzamento_comparecimento_perfil.R")

# =============================================================================
# 08_testes_padroes_espaciais_ml.R - Novos testes 9 a 12 (dados abertos)
#
#   N9.  Picos de percentuais (RKD, Rozenas 2017): excesso de seções com
#        percentual exatamente "redondo" (múltiplos de 5%), comparado com o
#        que se espera dado o tamanho de cada seção. Implementação própria do
#        método de densidade reamostrada (o pacote 'spikes' saiu do CRAN):
#        cada seção é reamostrada como Binomial(n, p_obs)/n, o que espalha
#        picos artificiais e preserva os naturais da grade de frações.
#   N10. Autocorrelação espacial (Moran local / LISA): agrupamentos de locais
#        de votação vizinhos com resíduos extremos dos testes de turno (T6),
#        perfil (N7) e painel (N8). 8 vizinhos mais próximos na mesma UF,
#        p-valor por permutação condicional e correção BH.
#   N11. Aprendizado de máquina (Zhang, Alvarez e Levin, 2019): floresta
#        aleatória treinada em seções SINTÉTICAS limpas e manipuladas
#        (enchimento de urna, troca de votos, números fabricados), aplicada às
#        seções reais. Compara o nº de seções reais classificadas como
#        "manipuladas" com a taxa de falso positivo medida nas sintéticas.
#   N12. Multimodalidade local (teste dip de Hartigan) por município, com
#        correção BH, em 2026 e, como controle, em 2022.
#
# Pré-requisito: 07 rodado (gera resultados/07_residuos_secao.csv).
# =============================================================================
source("00_config.R")
for (p in c("FNN", "ranger", "diptest"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
set.seed(2026)

cand_a <- 13L; cand_b <- 22L
n_reamostras <- 500     # N9
focais       <- seq(0.05, 0.95, by = 0.05)
meia_janela  <- 0.0005  # N9: |p - foco| < 0,05 p.p.
k_vizinhos   <- 8       # N10
n_perm       <- 199     # N10
alfa_lisa    <- 0.01    # N10 (q-valor)
n_treino     <- 150000  # N11: seções sintéticas de cada classe
fpr_alvo     <- 0.001   # N11: taxa de falso positivo tolerada nas sintéticas
min_secoes_dip <- 20    # N12
alfa_dip     <- 0.05    # N12 (q-valor)

etapa("Leitura e teste de carga")
pres <- ler_presidente()
lg <- largo_finalistas(pres, cand_a, cand_b)
det <- ler_detalhe()
if (is.null(det)) stop("Detalhe da apuração não encontrado. Rode 01_baixar_dados.R.")
lg <- merge(lg, det, by = chave, all.x = TRUE)
lg[, `:=`(n2 = lula + flavio, mun = paste(SG_UF, CD_MUNICIPIO),
          local = paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO))]
arq_res <- file.path(pasta_saida, "07_residuos_secao.csv")
res <- if (file.exists(arq_res)) fread(arq_res) else NULL
if (is.null(res)) message("Aviso: ", arq_res, " não encontrado; N10 usará só o resíduo de turno.")

# =============================================================================
# N9. RKD - picos em percentuais redondos
# =============================================================================
etapa("N9. Picos de percentuais redondos (RKD)")
massa_focal <- function(s) {          # nº de seções em cada percentual redondo
  k <- round(s * 20)                    # múltiplo de 5% mais próximo (1 a 19)
  perto <- abs(s - k / 20) < meia_janela & k >= 1 & k <= 19
  tabulate(k[perto], nbins = 19)
}
n9 <- list(); n9_focos <- list()
series <- list(
  list(turno = 1L, rot = "Lula (% válidos)",         y = "lula",   n = "validos"),
  list(turno = 1L, rot = "Flávio (% válidos)",       y = "flavio", n = "validos"),
  list(turno = 2L, rot = "Lula (% válidos)",         y = "lula",   n = "validos"),
  list(turno = 2L, rot = "Flávio (% válidos)",       y = "flavio", n = "validos"),
  list(turno = 1L, rot = "Comparecimento (% aptos)", y = "QT_COMPARECIMENTO", n = "QT_APTOS"),
  list(turno = 2L, rot = "Comparecimento (% aptos)", y = "QT_COMPARECIMENTO", n = "QT_APTOS"))
for (s in series) {
  x <- lg[NR_TURNO == s$turno, .(y = get(s$y), n = get(s$n))][!is.na(n) & !is.na(y) & n >= 20 & y <= n]
  obs <- massa_focal(x$y / x$n)
  rea <- replicate(n_reamostras, massa_focal(rbinom(nrow(x), x$n, x$y / x$n) / x$n))
  tot_obs <- sum(obs); tot_rea <- colSums(rea)
  n9[[length(n9) + 1]] <- data.table(
    turno = s$turno, serie = s$rot, secoes = nrow(x),
    secoes_em_focos_obs = tot_obs, esperado = round(mean(tot_rea), 1),
    excesso = round(tot_obs - mean(tot_rea), 1),
    excesso_pct_secoes = round(100 * (tot_obs - mean(tot_rea)) / nrow(x), 4),
    ic95_inf = round(100 * (tot_obs - quantile(tot_rea, 0.975)) / nrow(x), 4),
    ic95_sup = round(100 * (tot_obs - quantile(tot_rea, 0.025)) / nrow(x), 4),
    p_empirico = (1 + sum(tot_rea >= tot_obs)) / (n_reamostras + 1))
  n9_focos[[length(n9_focos) + 1]] <- data.table(turno = s$turno, serie = s$rot, foco_pct = 100 * focais,
                                                 observado = obs, esperado = round(rowMeans(rea), 1))
}
n9 <- rbindlist(n9); n9_focos <- rbindlist(n9_focos)
print(n9)
salvar(n9, "N9_rkd_resumo.csv"); salvar(n9_focos, "N9_rkd_por_foco.csv")
png(file.path(pasta_saida, "N9_rkd_focos.png"), width = 1200, height = 800)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (g in split(n9_focos, by = c("turno", "serie"))) {
  plot(g$foco_pct, g$observado, type = "h", lwd = 6, col = "grey40",
       ylim = c(0, max(c(g$observado, g$esperado)) * 1.1),
       xlab = "Percentual redondo", ylab = "Seções", main = sprintf("%dº turno - %s", g$turno[1], g$serie[1]))
  points(g$foco_pct, g$esperado, pch = 19, col = "red")
}
legend("topright", c("observado", "esperado (reamostragem)"), col = c("grey40", "red"), lwd = c(6, NA), pch = c(NA, 19))
dev.off()

# =============================================================================
# N10. Autocorrelação espacial (LISA)
# =============================================================================
etapa("N10. Autocorrelação espacial (Moran local)")
arqs_loc <- list.files(file.path(pasta_extra, "locais"), "\\.csv$", full.names = TRUE)
if (!length(arqs_loc)) {
  message("  N10 pulado: locais de votação não encontrados (rode 06).")
} else {
  loc <- rbindlist(lapply(arqs_loc, ler_csv_tse,
                          colunas = c("NR_TURNO", sec, "NR_LOCAL_VOTACAO", "NR_LATITUDE", "NR_LONGITUDE")),
                   fill = TRUE)
  if ("NR_TURNO" %in% names(loc) && 2L %in% as.integer(loc$NR_TURNO)) loc <- loc[as.integer(NR_TURNO) == 2L]
  num <- function(v) suppressWarnings(as.numeric(sub(",", ".", as.character(v), fixed = TRUE)))
  loc[, `:=`(lat = num(NR_LATITUDE), lon = num(NR_LONGITUDE))]
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(loc, j = cc, value = as.integer(loc[[cc]]))
  loc <- unique(loc[, c(sec, "lat", "lon"), with = FALSE], by = sec)
  base <- if (!is.null(res)) res else lg[NR_TURNO == 2, c(sec, "NR_LOCAL_VOTACAO", "n2"), with = FALSE]
  base <- merge(base, loc, by = sec)
  base <- base[!is.na(lat) & !is.na(lon) & lat != -1 & lon != -1 & abs(lat) <= 90 & abs(lon) <= 180]
  message("  Seções com coordenadas válidas: ", nrow(base))
  vars <- intersect(c("z_turno", "z_painel", "z_perfil"), names(base))
  ag <- base[, c(list(lat = mean(lat), lon = mean(lon), secoes = .N),
                 lapply(.SD, function(v) if (all(is.na(v))) NA_real_ else weighted.mean(v, n2, na.rm = TRUE))),
             by = .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO), .SDcols = vars]
  lisa <- list(); global <- list()
  for (v in vars) for (uf in setdiff(sort(unique(ag$SG_UF)), "ZZ")) {
    a <- ag[SG_UF == uf & !is.na(get(v))]
    if (nrow(a) <= k_vizinhos + 1) next
    xy <- cbind((a$lon - mean(a$lon)) * cos(mean(a$lat) * pi / 180) * 111.32, (a$lat - mean(a$lat)) * 110.57)
    nb <- FNN::get.knn(xy, k = k_vizinhos)$nn.index
    z <- (a[[v]] - mean(a[[v]])) / sd(a[[v]])
    lag <- rowMeans(matrix(z[nb], ncol = k_vizinhos))
    Ii <- z * lag
    cont <- integer(nrow(a))
    for (r in seq_len(n_perm)) {   # permutação condicional: vizinhos sorteados entre os demais
      idx <- matrix(sample.int(nrow(a) - 1L, nrow(a) * k_vizinhos, replace = TRUE), ncol = k_vizinhos)
      idx <- idx + (idx >= seq_len(nrow(a)))          # exclui o próprio local
      Ip <- z * rowMeans(matrix(z[idx], ncol = k_vizinhos))
      cont <- cont + (abs(Ip) >= abs(Ii))
    }
    a[, `:=`(variavel = v, I_local = Ii, z_std = z, lag = lag, p_perm = (1 + cont) / (n_perm + 1))]
    lisa[[length(lisa) + 1]] <- a
    global[[length(global) + 1]] <- data.table(variavel = v, SG_UF = uf, locais = nrow(a),
                                               moran_I_global = sum(z * lag) / sum(z^2))
  }
  lisa <- rbindlist(lisa, fill = TRUE); global <- rbindlist(global)
  lisa[, q := bh(p_perm), by = variavel]
  lisa[, tipo := fifelse(q >= alfa_lisa, "não significativo",
                 fifelse(z_std > 0 & lag > 0, "alto-alto",
                 fifelse(z_std < 0 & lag < 0, "baixo-baixo",
                 fifelse(z_std > 0, "alto-baixo", "baixo-alto"))))]
  # resumo: o que interessa é agrupamento de resíduos EXTREMOS (|z do local| > 3)
  resumo10 <- lisa[, .(locais = .N, significativos = sum(q < alfa_lisa),
                       alto_alto = sum(tipo == "alto-alto"), baixo_baixo = sum(tipo == "baixo-baixo"),
                       extremos_agrupados = sum(q < alfa_lisa & abs(z_std) > 3 & sign(lag) == sign(z_std))),
                   by = variavel]
  print(resumo10)
  print(global[, .(moran_I_mediano = round(median(moran_I_global), 3),
                   minimo = round(min(moran_I_global), 3), maximo = round(max(moran_I_global), 3)), by = variavel])
  salvar(resumo10, "N10_lisa_resumo.csv")
  salvar(global, "N10_moran_global_por_uf.csv")
  salvar(lisa[q < alfa_lisa & abs(z_std) > 3 & sign(lag) == sign(z_std)][order(variavel, -abs(z_std))],
         "N10_locais_extremos_agrupados.csv")
}

# =============================================================================
# N11. Floresta aleatória treinada com dados sintéticos
# =============================================================================
etapa("N11. Detecção por aprendizado de máquina (dados sintéticos)")
r <- merge(lg[NR_TURNO == 2], lg[NR_TURNO == 1, c(sec, "lula", "flavio", "QT_COMPARECIMENTO", "QT_APTOS"), with = FALSE],
           by = sec, suffixes = c("", "_1"))
r <- r[!is.na(QT_APTOS) & QT_APTOS >= 50 & QT_COMPARECIMENTO > 0 & n2 > 0 & (lula_1 + flavio_1) > 0]
r[, `:=`(comp_1 = QT_COMPARECIMENTO_1 / QT_APTOS_1, s_1 = lula_1 / (lula_1 + flavio_1),
         frac_validos = validos / total)]
# Referências locais tiradas dos dados reais (iguais para reais e sintéticas)
r[, `:=`(med_comp_local = median(QT_COMPARECIMENTO / QT_APTOS), med_s_local = median(lula / n2)), by = local]
r[, `:=`(med_dcomp_mun = median(QT_COMPARECIMENTO / QT_APTOS - comp_1), med_ds_mun = median(lula / n2 - s_1)), by = mun]

caracteristicas <- function(aptos, comp, lula, flavio, brnul, d) {
  validos <- lula + flavio
  s <- ifelse(validos > 0, lula / validos, 0.5)
  data.table(comparec = comp / aptos, s = s,
             d_comp_local = comp / aptos - d$med_comp_local,
             d_s_local = s - d$med_s_local,
             d_comp_turno = (comp / aptos - d$comp_1) - d$med_dcomp_mun,
             d_s_turno = (s - d$s_1) - d$med_ds_mun,
             brancos_nulos = ifelse(comp > 0, brnul / comp, 0),
             dig_lula = lula %% 10, dig_flavio = flavio %% 10,
             redondo = as.integer(abs(100 * s - round(100 * s)) < 0.05),
             aptos = aptos)
}
# Seção sintética limpa = 1º turno REAL da seção + mudança típica entre turnos
# (mediana do município + desvio sorteado da distribuição real de desvios).
# Assim a sintética mantém o perfil persistente de cada seção.
dev_comp <- r[, (QT_COMPARECIMENTO / QT_APTOS - comp_1) - med_dcomp_mun]
dev_s    <- r[, (lula / n2 - s_1) - med_ds_mun]
gerar_limpas <- function(d) {
  pi_c <- d$comp_1 + d$med_dcomp_mun + sample(dev_comp, nrow(d), replace = TRUE)
  th   <- d$s_1 + d$med_ds_mun + sample(dev_s, nrow(d), replace = TRUE)
  comp <- rbinom(nrow(d), d$QT_APTOS, pmin(pmax(pi_c, 0.01), 0.999))
  val  <- rbinom(nrow(d), comp, d$frac_validos)
  lula <- rbinom(nrow(d), val, pmin(pmax(th, 0.001), 0.999))
  list(aptos = d$QT_APTOS, comp = comp, lula = lula, flavio = val - lula, brnul = comp - val)
}
manipular <- function(L) {
  n <- length(L$comp); tipo <- sample(1:3, n, replace = TRUE)
  benef_lula <- runif(n) < 0.5
  # 1 enchimento: votos acrescentados a partir das abstenções
  i <- which(tipo == 1); k <- floor(runif(length(i), 0.1, 0.6) * (L$aptos[i] - L$comp[i]))
  L$comp[i] <- L$comp[i] + k
  L$lula[i] <- L$lula[i] + ifelse(benef_lula[i], k, 0); L$flavio[i] <- L$flavio[i] + ifelse(benef_lula[i], 0, k)
  # 2 troca: votos de um candidato passados ao outro
  i <- which(tipo == 2); f <- runif(length(i), 0.05, 0.3)
  m <- floor(f * ifelse(benef_lula[i], L$flavio[i], L$lula[i]))
  L$lula[i] <- L$lula[i] + ifelse(benef_lula[i], m, -m); L$flavio[i] <- L$flavio[i] + ifelse(benef_lula[i], -m, m)
  # 3 fabricação: totais redondos (percentual múltiplo de 5% e final 0)
  i <- which(tipo == 3); v <- L$lula[i] + L$flavio[i]
  alvo <- round(round(20 * L$lula[i] / pmax(v, 1)) / 20 * v, -1)
  L$lula[i] <- pmin(pmax(alvo, 0), v); L$flavio[i] <- v - L$lula[i]
  L$tipo <- tipo
  L
}
auc <- function(prob, y) {
  rk <- rank(prob); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(rk[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
idx_l <- sample(nrow(r), min(n_treino, nrow(r)), replace = n_treino > nrow(r))
idx_m <- sample(nrow(r), min(n_treino, nrow(r)), replace = n_treino > nrow(r))
L0 <- gerar_limpas(r[idx_l]); L1 <- manipular(gerar_limpas(r[idx_m]))
X0 <- do.call(caracteristicas, c(L0, list(d = r[idx_l])))
X1 <- do.call(caracteristicas, c(L1[c("aptos", "comp", "lula", "flavio", "brnul")], list(d = r[idx_m])))
sint <- rbind(X0[, classe := 0L], X1[, classe := 1L])
sint[, tipo := c(rep(0L, nrow(X0)), L1$tipo)]
teste <- sample(c(TRUE, FALSE), nrow(sint), replace = TRUE, prob = c(0.2, 0.8))
vars11 <- setdiff(names(sint), c("classe", "tipo"))
rf <- ranger::ranger(x = sint[!teste, vars11, with = FALSE], y = factor(sint$classe[!teste]),
                     num.trees = 300, probability = TRUE, importance = "impurity",
                     num.threads = max(1, parallel::detectCores() - 1), seed = 2026)
p_teste <- predict(rf, sint[teste, vars11, with = FALSE])$predictions[, "1"]
y_teste <- sint$classe[teste]
limiar <- quantile(p_teste[y_teste == 0], 1 - fpr_alvo, names = FALSE)
limiar1 <- quantile(p_teste[y_teste == 0], 0.99, names = FALSE)
tp_teste <- sint$tipo[teste]
val11 <- data.table(auc = auc(p_teste, y_teste),
                    auc_enchimento = auc(p_teste[tp_teste %in% c(0, 1)], y_teste[tp_teste %in% c(0, 1)]),
                    auc_troca      = auc(p_teste[tp_teste %in% c(0, 2)], y_teste[tp_teste %in% c(0, 2)]),
                    auc_fabricacao = auc(p_teste[tp_teste %in% c(0, 3)], y_teste[tp_teste %in% c(0, 3)]),
                    sensibilidade_fpr1pct = mean(p_teste[y_teste == 1] >= limiar1),
                    limiar = limiar,
                    fpr_sinteticas = mean(p_teste[y_teste == 0] >= limiar),
                    sensibilidade_enchimento = mean(p_teste[y_teste == 1 & sint$tipo[teste] == 1] >= limiar),
                    sensibilidade_troca      = mean(p_teste[y_teste == 1 & sint$tipo[teste] == 2] >= limiar),
                    sensibilidade_fabricacao = mean(p_teste[y_teste == 1 & sint$tipo[teste] == 3] >= limiar))
message("  Validação nas seções sintéticas (20% separadas):")
print(val11)
Xr <- caracteristicas(r$QT_APTOS, r$QT_COMPARECIMENTO, r$lula, r$flavio, r$total - r$validos, r)
r[, prob_manip := predict(rf, Xr[, vars11, with = FALSE])$predictions[, "1"]]
res11 <- data.table(secoes_reais = nrow(r), sinalizadas = sum(r$prob_manip >= limiar),
                    esperado_se_limpas = round(fpr_alvo * nrow(r), 1))
res11[, razao_obs_esp := round(sinalizadas / esperado_se_limpas, 2)]
print(res11)
imp <- data.table(variavel = names(rf$variable.importance), importancia = round(rf$variable.importance, 1))[order(-importancia)]
salvar(cbind(val11, res11), "N11_ml_resumo.csv")
salvar(imp, "N11_ml_importancia.csv")
salvar(r[prob_manip >= limiar, c(sec, "NM_MUNICIPIO", "NM_LOCAL_VOTACAO", "QT_APTOS", "QT_COMPARECIMENTO",
                                 "lula", "flavio", "prob_manip"), with = FALSE][order(-prob_manip)],
       "N11_secoes_sinalizadas.csv")
png(file.path(pasta_saida, "N11_ml_probabilidades.png"), width = 900, height = 600)
hist(r$prob_manip, breaks = 50, col = "grey70", border = "white", main = "Probabilidade de manipulação (seções reais, 2º turno)",
     xlab = "Probabilidade estimada pela floresta aleatória", ylab = "Seções")
abline(v = limiar, col = "red", lty = 2)
dev.off()

# =============================================================================
# N12. Multimodalidade por município (dip de Hartigan)
# =============================================================================
etapa("N12. Multimodalidade local (dip de Hartigan)")
dip_municipios <- function(dt, rotulo) {
  out <- dt[, if (.N >= min_secoes_dip) {
                tst <- diptest::dip.test(share)
                list(secoes = .N, dip = unname(tst$statistic), p = tst$p.value)
              }, by = .(SG_UF, CD_MUNICIPIO)]
  out[, `:=`(q = bh(p), ano = rotulo)]
}
d26 <- lg[NR_TURNO == 2 & n2 >= 20, .(SG_UF, CD_MUNICIPIO, share = lula / n2)]
m26 <- dip_municipios(d26, "2026")
arq22 <- list.files(file.path(pasta_extra, "votacao2022"), "_BR\\.csv$", full.names = TRUE)
m22 <- NULL
if (length(arq22)) {
  v22 <- ler_csv_tse(arq22[1], c(sec, "NR_TURNO", "DS_CARGO", "NR_VOTAVEL", "QT_VOTOS"))
  if ("DS_CARGO" %in% names(v22)) v22 <- v22[toupper(DS_CARGO) == "PRESIDENTE"]
  for (cc in c("NR_TURNO", "NR_VOTAVEL", "QT_VOTOS", "CD_MUNICIPIO")) set(v22, j = cc, value = as.integer(v22[[cc]]))
  d22 <- v22[NR_TURNO == 2, .(l = sum(QT_VOTOS[NR_VOTAVEL == 13]), b = sum(QT_VOTOS[NR_VOTAVEL == 22])), by = sec][
    l + b >= 20, .(SG_UF, CD_MUNICIPIO, share = l / (l + b))]
  m22 <- dip_municipios(d22, "2022")
}
dip <- rbind(m26, m22)
res12 <- dip[, .(municipios_testados = .N, multimodais = sum(q < alfa_dip)), by = ano]
if (!is.null(m22)) {
  amb <- merge(m26[, .(SG_UF, CD_MUNICIPIO, mm26 = q < alfa_dip)], m22[, .(SG_UF, CD_MUNICIPIO, mm22 = q < alfa_dip)],
               by = c("SG_UF", "CD_MUNICIPIO"))
  message(sprintf("  Municípios nos dois anos: %d | multimodais em 2026: %d, dos quais também em 2022: %d | só em 2026: %d",
                  nrow(amb), amb[mm26 == TRUE, .N], amb[mm26 & mm22, .N], amb[mm26 & !mm22, .N]))
  dip <- merge(dip, amb, by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)
}
print(res12)
nomes_mun <- unique(lg[, .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)])
salvar(res12, "N12_dip_resumo.csv")
salvar(merge(dip[q < alfa_dip], nomes_mun, by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)[order(ano, q)],
       "N12_municipios_multimodais.csv")

message("\nConcluído. Próximo passo (opcional, arquivos de urna): 09_arquivos_urna.py")

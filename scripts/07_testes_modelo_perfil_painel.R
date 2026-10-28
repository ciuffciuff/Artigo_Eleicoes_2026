# =============================================================================
# 07_testes_modelo_perfil_painel.R - Novos testes 6, 7 e 8 (dados abertos)
#
#   N6. Modelo da urna: o voto não deve depender do modelo do equipamento.
#       Regressão com efeito fixo de LOCAL de votação (seções da mesma escola
#       com urnas de modelos diferentes) e erro-padrão por município.
#       Esperado: coeficientes do modelo próximos de zero.
#   N7. Perfil do eleitorado: o voto da seção deve ser coerente com sexo,
#       idade, escolaridade e biometria dos eleitores. Modelo logístico com
#       efeito fixo de município e superdispersão beta-binomial (estimada por
#       momentos), com verificação preditiva por simulação. É uma aproximação
#       do modelo hierárquico proposto, viável para ~500 mil seções.
#   N8. Painel 2022 -> 2026: mudança do voto na mesma seção entre as eleições,
#       centrada no município. Mudanças muito fora do padrão local são
#       sinalizadas.
#
# Também grava 07_residuos_secao.csv, usado pelo script 08.
# Pré-requisito: 01, 02 e 06 rodados com sucesso.
# =============================================================================
source("00_config.R")
set.seed(2026)

cand_a <- 13L; cand_b <- 22L   # Lula e Flávio Bolsonaro (2026)
alfa_fdr  <- 0.001             # q-valor máximo para sinalizar seção (N7)
z_limite  <- 6                 # |z| robusto mínimo para sinalizar (N8)
min_votos <- 50                # seções menores ficam fora de N7/N8
n_rep     <- 200               # simulações da verificação preditiva (N7)

sem_acento <- function(x) toupper(chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇáàâãäéèêëíìîïóòôõöúùûüç",
                                         "AAAAAEEEEIIIIOOOOOUUUUCaaaaaeeeeiiiiooooouuuuc", x))

# ---- Leitura e teste de carga ---------------------------------------------------
etapa("Leitura e teste de carga")
pres <- ler_presidente()
lg <- largo_finalistas(pres, cand_a, cand_b)
det <- ler_detalhe(extras = c("DS_MODELO_URNA", "DS_ORIGEM_VOTO", "ST_SECAO_ANULADA"))
if (is.null(det)) stop("Detalhe da apuração não encontrado. Rode 01_baixar_dados.R.")
lg <- merge(lg, det, by = chave, all.x = TRUE)
lg[, `:=`(mun = paste(SG_UF, CD_MUNICIPIO), local = paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO),
          n2 = lula + flavio)]
message("  Seções x turno: ", nrow(lg), " | sem detalhe: ", lg[is.na(QT_APTOS), .N])

# =============================================================================
# N6. Modelo da urna
# =============================================================================
etapa("N6. Modelo da urna")
res6 <- list(); desc6 <- NULL
if (!"DS_MODELO_URNA" %in% names(lg) || all(is.na(lg$DS_MODELO_URNA))) {
  message("  N6 pulado: o detalhe não traz DS_MODELO_URNA.")
} else {
  d6 <- copy(lg)
  d6[, modelo := gsub("\\s+", "", toupper(DS_MODELO_URNA))]
  if ("DS_ORIGEM_VOTO" %in% names(d6))
    d6 <- d6[is.na(DS_ORIGEM_VOTO) | grepl("URNA", sem_acento(DS_ORIGEM_VOTO))]
  d6 <- d6[!is.na(modelo) & modelo != "" & !modelo %in% c("#NULO#", "#NE#", "NAOSEAPLICA")]
  desc6 <- d6[, .(secoes = .N, lula_pct = round(100 * sum(lula) / sum(n2), 2),
                  comparec_pct = round(100 * sum(QT_COMPARECIMENTO, na.rm = TRUE) / sum(QT_APTOS, na.rm = TRUE), 2)),
              by = .(NR_TURNO, modelo)][order(NR_TURNO, modelo)]
  message("  Distribuição bruta (sem controle de lugar) - diferenças aqui refletem geografia:")
  print(desc6)
  ref <- d6[, .N, by = modelo][which.max(N), modelo]
  message("  Modelo de referência (mais comum): ", ref)
  mods <- setdiff(sort(unique(d6$modelo)), ref)
  var_loc <- d6[, uniqueN(modelo), by = .(NR_TURNO, local)][, .(locais_com_2_ou_mais_modelos = sum(V1 > 1)), by = NR_TURNO]
  print(var_loc)
  for (tr in sort(unique(d6$NR_TURNO))) {
    x <- d6[NR_TURNO == tr & n2 > 0]
    X <- sapply(mods, function(m) as.numeric(x$modelo == m)); colnames(X) <- paste0("modelo_", mods)
    desf <- list(
      lula_logit   = list(y = logit_emp(x$lula, x$n2), w = x$n2),
      comparec_pct = list(y = 100 * x$QT_COMPARECIMENTO / x$QT_APTOS, w = x$QT_APTOS),
      brancos_nulos_pct = list(y = 100 * (x$total - x$validos) / x$total, w = x$total))
    for (nm in names(desf)) {
      r <- fe_lm(desf[[nm]]$y, X, fe = x$local, cl = x$mun, w = desf[[nm]]$w)
      r[, `:=`(turno = tr, desfecho = nm, referencia = ref)]
      res6[[length(res6) + 1]] <- r
    }
  }
  res6 <- rbindlist(res6)
  res6[, q := bh(p)]
  setcolorder(res6, c("turno", "desfecho", "termo", "referencia"))
  message("  Efeito do modelo DENTRO do mesmo local de votação (esperado: perto de zero):")
  print(res6[, .(turno, desfecho, termo, coef = round(coef, 4), ep = round(ep, 4), p = signif(p, 3), q = signif(q, 3), n)])
  salvar(desc6, "N6_modelo_urna_bruto.csv")
  salvar(res6,  "N6_modelo_urna_efeito_fixo.csv")
}

# =============================================================================
# N7. Perfil do eleitorado
# =============================================================================
etapa("N7. Perfil do eleitorado por seção")
arqs_perfil <- list.files(file.path(pasta_extra, "perfil"), "\\.csv$", full.names = TRUE)
perfil <- NULL
if (!length(arqs_perfil)) {
  message("  N7 pulado: perfil não encontrado (rode 06_baixar_dados_abertos_extra.R).")
} else {
  col_p <- c(sec, "DS_GENERO", "DS_FAIXA_ETARIA", "DS_GRAU_ESCOLARIDADE",
             "QT_ELEITORES_PERFIL", "QT_ELEITORES_BIOMETRIA")
  ler_perfil_uf <- function(f) {
    message("  Lendo ", basename(f))
    p <- ler_csv_tse(f, c(col_p, "QT_ELEITORES"))
    # em 2026 a contagem chama QT_ELEITORES (QT_ELEITORES_PERFIL em anos anteriores)
    if (!"QT_ELEITORES_PERFIL" %in% names(p) && "QT_ELEITORES" %in% names(p))
      setnames(p, "QT_ELEITORES", "QT_ELEITORES_PERFIL")
    falta <- setdiff(c(sec, "QT_ELEITORES_PERFIL"), names(p))
    if (length(falta)) stop("Perfil sem as colunas: ", paste(falta, collapse = ", "), " em ", basename(f))
    for (cc in setdiff(col_p, names(p))) p[, (cc) := NA]
    p[, `:=`(gen = sem_acento(DS_GENERO), esc = sem_acento(DS_GRAU_ESCOLARIDADE),
             idade = suppressWarnings(as.integer(sub("^\\D*(\\d+).*$", "\\1", DS_FAIXA_ETARIA))))]
    p[, .(aptos_perfil = sum(QT_ELEITORES_PERFIL),
          p_fem   = sum(QT_ELEITORES_PERFIL[gen %like% "FEMININO"]),
          p_16_24 = sum(QT_ELEITORES_PERFIL[!is.na(idade) & idade < 25]),
          p_45_59 = sum(QT_ELEITORES_PERFIL[!is.na(idade) & idade >= 45 & idade < 60]),
          p_60m   = sum(QT_ELEITORES_PERFIL[!is.na(idade) & idade >= 60]),
          p_esc_baixa = sum(QT_ELEITORES_PERFIL[esc %like% "ANALFABETO|LE E ESCREVE|FUNDAMENTAL INCOMPLETO"]),
          p_esc_sup   = sum(QT_ELEITORES_PERFIL[esc %like% "SUPERIOR"]),
          p_bio   = sum(as.numeric(QT_ELEITORES_BIOMETRIA), na.rm = TRUE)),
      by = sec]
  }
  perfil <- rbindlist(lapply(arqs_perfil, ler_perfil_uf))
  vars7 <- c("p_fem", "p_16_24", "p_45_59", "p_60m", "p_esc_baixa", "p_esc_sup", "p_bio")
  for (v in vars7) set(perfil, j = v, value = perfil[[v]] / perfil$aptos_perfil)
  if (all(perfil$p_bio == 0)) { perfil[, p_bio := NULL]; vars7 <- setdiff(vars7, "p_bio") }
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(perfil, j = cc, value = as.integer(perfil[[cc]]))
  message("  Perfil: ", nrow(perfil), " seções; ",
          perfil[, sum(p_fem == 0 & p_esc_sup == 0)], " sem categorias reconhecidas")
  salvar(perfil[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 4)), .SDcols = vars7],
         "N7_perfil_medias.csv")
}

rbetabinom <- function(n, p, rho) {
  if (rho <= 1e-8) return(rbinom(length(n), n, p))
  a <- p * (1 / rho - 1); b <- (1 - p) * (1 / rho - 1)
  rbinom(length(n), n, rbeta(length(n), a, b))
}

res7 <- list(); ppc7 <- list(); flag7 <- list(); r7_secao <- list()
if (!is.null(perfil)) {
  alvos <- list(list(turno = 2L, cand = cand_a, rot = "Lula (2º turno, % dos 2 finalistas)"),
                list(turno = 1L, cand = cand_a, rot = "Lula (1º turno, % dos válidos)"),
                list(turno = 1L, cand = cand_b, rot = "Flávio (1º turno, % dos válidos)"))
  for (a in alvos) {
    x <- merge(lg[NR_TURNO == a$turno], perfil, by = sec)
    x[, y := if (a$cand == cand_a) lula else flavio]
    x[, n := if (a$turno == 2L) n2 else validos]
    x <- x[n >= min_votos & complete.cases(x[, ..vars7])]
    message(sprintf("  %s: %d seções no modelo", a$rot, nrow(x)))
    x[, ly := logit_emp(y, n)]
    coefs <- fe_lm(x$ly, x[, ..vars7], fe = x$mun, cl = x$mun, w = x$n)
    b <- setNames(coefs$coef, coefs$termo)
    xb <- as.matrix(x[, names(b), with = FALSE]) %*% b
    x[, xb := xb[, 1]]
    x[, alfa := sum((ly - xb) * n) / sum(n), by = mun]          # efeito fixo do município
    x[, p_hat := plogis(xb + alfa)]
    # superdispersão (método dos momentos): E[(y-np)^2] = n p (1-p) [1 + (n-1) rho]
    x[, r2 := (y - n * p_hat)^2 / (n * p_hat * (1 - p_hat))]
    rho <- max(0, sum((x$r2 - 1) * (x$n - 1)) / sum((x$n - 1)^2))
    x[, z := (y - n * p_hat) / sqrt(n * p_hat * (1 - p_hat) * (1 + (n - 1) * rho))]
    x[, p_valor := 2 * pnorm(-abs(z))]
    x[, q := bh(p_valor)]
    r2_within <- 1 - sum(x$n * (x$ly - x$xb - x$alfa)^2) /
                     sum(x$n * (x$ly - ave(x$ly * x$n, x$mun, FUN = sum) / ave(x$n, x$mun, FUN = sum))^2)
    coefs[, `:=`(alvo = a$rot, rho = rho, r2_dentro_municipio = r2_within)]
    res7[[length(res7) + 1]] <- coefs
    # verificação preditiva: estatísticas observadas x simuladas pelo modelo
    est <- function(yv) {
      s <- yv / x$n
      c(secoes_com_zero = sum(yv == 0), secoes_acima_90pct = sum(s > 0.9),
        secoes_abaixo_10pct = sum(s < 0.1),
        dp_dentro_municipio = sqrt(sum(x$n * (s - ave(s * x$n, x$mun, FUN = sum) / ave(x$n, x$mun, FUN = sum))^2) / sum(x$n)))
    }
    obs <- est(x$y)
    sim <- t(replicate(n_rep, est(rbetabinom(x$n, x$p_hat, rho))))
    ppc7[[length(ppc7) + 1]] <- data.table(alvo = a$rot, estatistica = names(obs), observado = obs,
                                           simulado_media = colMeans(sim),
                                           simulado_p2.5 = apply(sim, 2, quantile, 0.025),
                                           simulado_p97.5 = apply(sim, 2, quantile, 0.975),
                                           p_preditivo = sapply(seq_along(obs), function(i)
                                             min(1, 2 * min(mean(sim[, i] >= obs[i]), mean(sim[, i] <= obs[i])))))
    message(sprintf("    rho = %.4f | R² dentro do município = %.3f | |z|>3: %.2f%% (normal: 0,27%%) | sinalizadas (q<%.3f): %d",
                    rho, r2_within, 100 * mean(abs(x$z) > 3), alfa_fdr, sum(x$q < alfa_fdr)))
    flag7[[length(flag7) + 1]] <- x[q < alfa_fdr, c("NR_TURNO", sec, "NM_MUNICIPIO", "NM_LOCAL_VOTACAO",
                                                    "y", "n", "p_hat", "z", "q"), with = FALSE][, alvo := a$rot]
    if (a$turno == 2L) r7_secao <- setnames(x[, c(sec, "z"), with = FALSE], "z", "z_perfil")
  }
  res7 <- rbindlist(res7); ppc7 <- rbindlist(ppc7); flag7 <- rbindlist(flag7)
  print(res7[, .(alvo, termo, coef = round(coef, 3), ep = round(ep, 3), rho = round(rho, 4))])
  message("  Verificação preditiva (p_preditivo pequeno = o modelo não reproduz o padrão observado):")
  print(ppc7[, .(alvo, estatistica, observado, simulado_media = round(simulado_media, 1), p_preditivo)])
  salvar(res7, "N7_perfil_coeficientes.csv")
  salvar(ppc7, "N7_verificacao_preditiva.csv")
  salvar(flag7[order(-abs(z))], "N7_secoes_sinalizadas.csv")
}

# =============================================================================
# N8. Painel 2022 -> 2026 (2º turno, mesma seção)
# =============================================================================
etapa("N8. Painel 2022 -> 2026")
arq22 <- list.files(file.path(pasta_extra, "votacao2022"), "_BR\\.csv$", full.names = TRUE)
r8_secao <- NULL
if (!length(arq22)) {
  message("  N8 pulado: votação 2022 não encontrada (rode 06_baixar_dados_abertos_extra.R).")
} else {
  v22 <- ler_csv_tse(arq22[1], c(sec, "NR_TURNO", "DS_CARGO", "NR_LOCAL_VOTACAO", "NR_VOTAVEL", "QT_VOTOS"))
  if ("DS_CARGO" %in% names(v22)) v22 <- v22[toupper(DS_CARGO) == "PRESIDENTE"]
  for (cc in c("NR_TURNO", "NR_VOTAVEL", "QT_VOTOS", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO"))
    set(v22, j = cc, value = as.integer(v22[[cc]]))
  message("  2022: ", v22[, uniqueN(paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO))], " seções; turnos ",
          paste(sort(unique(v22$NR_TURNO)), collapse = ", "))
  v22 <- v22[NR_TURNO == 2, .(lula22 = sum(QT_VOTOS[NR_VOTAVEL == 13]),
                              bolso22 = sum(QT_VOTOS[NR_VOTAVEL == 22]),
                              local22 = NR_LOCAL_VOTACAO[1]), by = sec]
  p8 <- merge(lg[NR_TURNO == 2], v22, by = sec)
  p8[, mesmo_local := as.integer(NR_LOCAL_VOTACAO) == as.integer(local22)]
  message(sprintf("  Pareadas por UF/município/zona/seção: %d de %d seções de 2026 (%.1f%%); no mesmo local: %d",
                  nrow(p8), lg[NR_TURNO == 2, .N], 100 * nrow(p8) / lg[NR_TURNO == 2, .N], p8[mesmo_local == TRUE, .N]))
  p8 <- p8[mesmo_local == TRUE & n2 >= min_votos & (lula22 + bolso22) >= min_votos]
  p8[, `:=`(s26 = lula / n2, s22 = lula22 / (lula22 + bolso22))]
  p8[, delta := logit_emp(lula, n2) - logit_emp(lula22, lula22 + bolso22)]
  p8[, resid := delta - median(delta), by = mun]
  p8[, n_mun := .N, by = mun]
  p8[, escala := 1.4826 * median(abs(resid)), by = SG_UF]
  p8[, z := resid / escala]
  cor8 <- p8[, .(secoes = .N, cor_pearson = cor(s22, s26), mudanca_media_pp = 100 * mean(s26 - s22),
                 sinalizadas = sum(abs(z) > z_limite & n_mun >= 5)), by = SG_UF][order(SG_UF)]
  cor8 <- rbind(p8[, .(SG_UF = "BRASIL", secoes = .N, cor_pearson = cor(s22, s26),
                       mudanca_media_pp = 100 * mean(s26 - s22),
                       sinalizadas = sum(abs(z) > z_limite & n_mun >= 5))], cor8)
  print(cor8)
  salvar(cor8, "N8_painel_resumo.csv")
  salvar(p8[abs(z) > z_limite & n_mun >= 5,
            c(sec, "NM_MUNICIPIO", "NM_LOCAL_VOTACAO", "lula22", "bolso22", "lula", "flavio", "s22", "s26", "z"),
            with = FALSE][order(-abs(z))], "N8_secoes_sinalizadas.csv")
  png(file.path(pasta_saida, "N8_painel_2022_2026.png"), width = 900, height = 850)
  smoothScatter(100 * p8$s22, 100 * p8$s26, nrpoints = 2000,
                xlab = "Lula 2022, 2º turno (% dos 2 finalistas)",
                ylab = "Lula 2026, 2º turno (% dos 2 finalistas)",
                main = sprintf("Mesma seção em 2022 e 2026 (n = %s; r = %.3f)",
                               format(nrow(p8), big.mark = ".", decimal.mark = ","), cor(p8$s22, p8$s26)))
  abline(0, 1, col = "red", lty = 2)
  dev.off()
  r8_secao <- setnames(p8[, c(sec, "z"), with = FALSE], "z", "z_painel")
}

# =============================================================================
# Resíduos por seção para o script 08
# =============================================================================
etapa("Resíduos por seção (para o 08)")
t1 <- lg[NR_TURNO == 1, c(sec, "lula", "flavio", "validos", "QT_APTOS", "QT_COMPARECIMENTO"), with = FALSE]
setnames(t1, c("lula", "flavio", "validos", "QT_APTOS", "QT_COMPARECIMENTO"),
         c("lula1", "flavio1", "validos1", "aptos1", "comp1"))
res <- merge(lg[NR_TURNO == 2], t1, by = sec, all.x = TRUE)
# consistência entre turnos (como o T6 do 04): Lula no 2º turno x Lula entre
# os dois finalistas no 1º turno, centrado no município
res[, d_turno := logit_emp(lula, n2) - logit_emp(lula1, lula1 + flavio1)]
res[n2 >= min_votos & (lula1 + flavio1) >= min_votos, r_turno := d_turno - median(d_turno), by = mun]
res[, z_turno := r_turno / (1.4826 * median(abs(r_turno), na.rm = TRUE)), by = SG_UF]
if (length(r7_secao)) res <- merge(res, r7_secao, by = sec, all.x = TRUE) else res[, z_perfil := NA_real_]
if (!is.null(r8_secao)) res <- merge(res, r8_secao, by = sec, all.x = TRUE) else res[, z_painel := NA_real_]
if (!is.null(perfil)) res <- merge(res, perfil, by = sec, all.x = TRUE)
res[, c("mun", "local", "d_turno", "r_turno") := NULL]
salvar(res, "07_residuos_secao.csv")
message("  ", nrow(res), " seções (2º turno) com resíduos dos testes de turno, perfil e painel.")

message("\nConcluído. Próximo passo: 08_testes_padroes_espaciais_ml.R")

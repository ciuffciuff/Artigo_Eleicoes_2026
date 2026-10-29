# =============================================================================
# 12_variacao_lula_esquerda.R - Novo teste 14 (N14): variação de Lula do
#                               1º para o 2º turno x votação da esquerda no 1º
# Versão 3 (29/10/2026) - mudanças em relação à v2:
#   a. Abstenção: o saldo de comparecimento vira entrada + saída, e a saída é
#      repartida na proporção do 1º turno da própria seção (ver X_cols).
#      Corrige o viés que punha resíduos a favor de Flávio nos redutos de
#      Lula que perderam eleitores (e vice-versa).
#   b. Tipo de local: padrões corrigidos ("LAR" e "PRESÍDIO" como palavras
#      inteiras; sem "CENTRO DE ATENDIMENTO"). O classificar_locais_t6.R
#      recebeu a mesma correção.
#   c. Teste D: nova tabela com a participação de Lula entre as seções
#      acima do teto, observada x simulada (N14_teto_participacao.csv).
#      Verificado com dados simulados: sem fraude, contagem e participação
#      ficam dentro do esperado (p 0,33-1). Com fraude SÓ a favor de um
#      lado, espalhada por 150-300 seções, D (contagem e participação) NÃO
#      a detecta (p 0,46-0,75), enquanto C detecta (19 x 0, p < 1e-5, com
#      15-30% dos votos trocados). Portanto D serve para EXPLICAR o número de
#      seções acima do teto de cada lado (o que o modelo espera), não para
#      detectar fraude; a detecção e a direção vêm de C, E e B.
#
# Versão 2 (29/10/2026) - mudanças em relação à v1:
#   1. Erro-padrão: no DF (um só "município") o cluster passa a ser a zona
#      eleitoral; na v1 o erro-padrão saía infinito.
#   2. Teste D: o p-valor deixa de usar a hipótese 50/50 (inválida, porque
#      Lula ganhou mais da reserva que Flávio e fica naturalmente mais perto
#      do teto). Agora o número esperado de seções acima do teto, para cada
#      lado, vem de SIMULAÇÃO com o modelo de transferência estimado, sorteando
#      os desvios dentro de UF x faixa do % previsto (testado com dados
#      sintéticos sem fraude: observado/esperado ~1 em todos os conjuntos).
#      LIMITAÇÃO (verificada com fraude simulada): como a referência é
#      estimada com os próprios dados, uma fraude ESPALHADA por muitas seções
#      entra também no "esperado" e o teste D não a detecta. Ele detecta
#      excesso CONCENTRADO num subconjunto (tipo de local, faixa de força da
#      esquerda). Para direção (a favor de quem), use C, E e B.
#   3. Presídios, aldeias, hospitais e exterior são separados das seções
#      comuns (mesmas palavras-chave do classificar_locais_t6.R). As
#      conclusões principais usam só as seções comuns.
#   4. Teste C: variância mínima (como se o % previsto fosse no máximo 95%),
#      para não inflar o z quando a previsão está perto de 0% ou 100%.
#
# Pergunta: o crescimento de Lula do 1º para o 2º turno, em cada seção, é
# compatível com o que havia de voto "disponível" na própria seção no 1º
# turno - principalmente o voto dos outros candidatos de esquerda?
#
# Testes:
#   A. Transferência de votos (regressão ecológica de Goodman), Brasil e UF,
#      erro-padrão por município (DF: por zona). Decompõe de onde veio o
#      voto de Lula no 2º turno.
#   B. Ganho de Lula (p.p.) ~ reserva de esquerda + centro/direita +
#      brancos/nulos + saldo de comparecimento, efeito fixo de município.
#      Coeficiente da reserva de esquerda > 1 seria implausível.
#   C. Probabilidade por seção: % de Lula no 2º turno x % previsto pelo
#      1º turno da própria seção; resíduo centrado no município, z robusto,
#      q-valor (BH). Simetria (binomial) por tipo de local.
#   D. Teto: Lula no 2º turno acima de tudo o que não era do adversário no
#      1º turno. Observado x esperado por simulação, para cada lado e
#      conjunto (detecta concentração; ver limitação acima).
#   E. Resultados por decil de força da esquerda no 1º turno + gráfico.
#   F. Validação: troca simulada de votos em 300 seções comuns (cópia em
#      memória), metade para cada lado.
#
# ATENÇÃO - como interpretar:
#   * Regressão ecológica estima médias, não o comportamento de cada eleitor
#     (taxas acima de 1 ou negativas indicam efeito de contexto regional).
#   * O saldo de comparecimento é LÍQUIDO; o teto (D) é "macio".
#   * Padrão incomum não é prova de fraude; o que pesa é assimetria
#     sistemática a favor de um lado, além do esperado.
#   * A classificação dos locais é APROXIMADA (palavras no nome).
#
# Saídas: resultados/N14_*.csv e N14_ganho_lula_por_decil_esquerda.png
# Pré-requisito: 01 e 02 rodados.
# =============================================================================
source("00_config.R")
set.seed(2026)

# ---- Parâmetros ----------------------------------------------------------------
n_lula     <- 13L
n_flavio   <- 22L
esq_outros <- c(16L, 21L, 29L, 80L)   # PSTU, PCB, PCO, UP - confira a classificação
min_votos  <- 30      # seções com menos votos em algum turno ficam de fora
min_sec    <- 3       # seções mínimas no município para centrar o resíduo
alfa_fdr   <- 0.001   # q-valor máximo (teste C)
dif_min_pp <- 10      # desvio mínimo em p.p. (teste C)
p_piso     <- 0.05    # teste C/D: variância mínima = p_piso * (1 - p_piso)
exc_min_votos <- 5    # teste D: excesso mínimo em votos ...
exc_min_frac  <- 0.03 # ... e em fração dos votos nos finalistas
n_sim_teto <- 200     # simulações do teste D (mais = p-valor mais preciso)
n_faixas   <- 20      # teste D: faixas do % previsto de Lula para o sorteio
min_estrato <- 50     # teste D: seções mínimas por estrato UF x faixa
n_validar  <- 300

# Modelo de transferência (v3): o saldo de comparecimento é dividido em
# ENTRADA (eleitores a mais no 2º turno) e SAÍDA (eleitores a menos),
# e a saída é repartida na proporção do voto da própria seção no 1º turno.
# Na v2 a saída usava uma taxa única (média nacional), o que gerava resíduos
# a favor de Flávio nos redutos de Lula que perderam eleitores, e a favor de
# Lula nos redutos de Flávio (verificado com dados simulados sem fraude).
X_cols  <- c("lula1", "flavio1", "esq1", "outros1", "bn1",
             "entrada", "saida_lula", "saida_flavio", "saida_outros")
rotulos <- c("votou em Lula", "votou em Flávio", "votou em outro de esquerda",
             "votou em outro (centro/direita)", "branco/nulo no 1º turno",
             "eleitor a mais no 2º turno (entrada)",
             "eleitor a menos: parte de Lula no 1º turno (saída)",
             "eleitor a menos: parte de Flávio no 1º turno (saída)",
             "eleitor a menos: parte dos demais no 1º turno (saída)")

# Tipo de local (palavras do classificar_locais_t6.R, corrigidas na v3).
# Nas posições de letra acentuada o padrão usa "..?" (aceita a letra com ou
# sem acento), o que funciona em qualquer configuração regional. "(^|[^A-Z])"
# e "([^A-Z]|$)" exigem palavra inteira: na v2, "LAR " pegava "ESCOLAR " e
# "PRESID" pegava "PRESIDENTE"; "CENTRO DE ATENDIMENTO" pegava os CAIC
# (escolas) e foi retirado (o socioeducativo é pego por SOCIOEDUC/CASE).
ini <- "(^|[^A-Z])"; fim <- "([^A-Z]|$)"
pal_local <- list(
  prisao   = paste0("PRES..?DIO|PENITENC|PRISION|CADEIA P..?BLICA|SOCIOEDUC|DETEN..?..?O|",
                    ini, "CDP|CUST..?DIA|", ini, "PENAL", fim, "|", ini, "APAC", fim, "|INTERNA..?..?O|",
                    "RESSOCIALIZ|", ini, "CASE", fim, "|FEBEM|FUNDA..?..?O CASA|CRIMINAL|CARCER|",
                    ini, "PRESOS?", fim),
  aldeia   = paste0("ALDEIA|IND..?GENA|TERRA IND|POLO BASE"),
  hospital = paste0("HOSPITAL|", ini, "ASILO|", ini, "ABRIGO|CASA DE REPOUSO|", ini, "ILPI", fim, "|",
                    ini, "LAR", fim, "|CL..?NICA|SANAT..?RIO|IDOSO"))
tipo_local <- function(nome, uf) {
  nm <- toupper(fifelse(is.na(nome), "", nome))
  tem <- function(p) grepl(p, nm, useBytes = TRUE)
  fcase(tem(pal_local$prisao), "presidio/socioeducativo",
        tem(pal_local$aldeia), "aldeia indigena",
        tem(pal_local$hospital), "hospital/abrigo",
        uf == "ZZ", "exterior",
        default = "comum")
}

# ---- Funções -------------------------------------------------------------------
# Uma linha por seção, com os dois turnos
montar <- function(pres) {
  por_secao <- function(tr) pres[NR_TURNO == tr, .(
    lula   = sum(QT_VOTOS[NR_VOTAVEL == n_lula]),
    flavio = sum(QT_VOTOS[NR_VOTAVEL == n_flavio]),
    esq    = sum(QT_VOTOS[NR_VOTAVEL %in% esq_outros]),
    bn     = sum(QT_VOTOS[NR_VOTAVEL %in% especiais]),
    tot    = sum(QT_VOTOS),
    NM_MUNICIPIO = NM_MUNICIPIO[1], NM_LOCAL_VOTACAO = NM_LOCAL_VOTACAO[1]), by = sec]
  s1 <- por_secao(1L); s1[, outros := tot - lula - flavio - esq - bn]
  s2 <- por_secao(2L)
  fora <- s2[, sum(as.numeric(tot - lula - flavio - bn))]
  if (fora != 0) stop("2º turno com ", fora, " votos fora dos dois finalistas e brancos/nulos")
  d <- merge(s1, s2[, c(sec, "lula", "flavio", "bn", "tot"), with = FALSE],
             by = sec, suffixes = c("1", "2"))
  setnames(d, c("esq", "outros"), c("esq1", "outros1"))
  d[, `:=`(v1 = tot1 - bn1, n2 = lula2 + flavio2, saldo = tot2 - tot1,
           mun = paste(SG_UF, CD_MUNICIPIO))]
  d <- d[tot1 >= min_votos & tot2 >= min_votos & v1 > 0 & n2 > 0]
  # saldo = entrada (>= 0) + saída (<= 0); saída repartida pelo 1º turno da seção
  d[, `:=`(entrada = pmax(saldo, 0L), saida = pmin(saldo, 0L))]
  d[, `:=`(saida_lula   = saida * lula1 / tot1,
           saida_flavio = saida * flavio1 / tot1,
           saida_outros = saida * (esq1 + outros1 + bn1) / tot1)]
  # cluster do erro-padrão: município; se a UF tem um só município (DF), zona
  d[, cl := if (uniqueN(mun) < 2) paste(mun, NR_ZONA) else mun, by = SG_UF]
  d[, tipo := tipo_local(NM_LOCAL_VOTACAO, SG_UF)]
  d[]
}

# MQO sem intercepto com erro-padrão robusto por cluster
ols_cl <- function(y, X, cl) {
  fit <- lm.fit(X, y)
  b <- fit$coefficients
  ok <- !is.na(b)                      # coluna sem variação/colinear: fica de fora
  b[!ok] <- 0
  ep <- rep(NA_real_, ncol(X))
  Xk <- X[, ok, drop = FALSE]
  u <- y - (X %*% b)[, 1]
  sc <- rowsum(Xk * u, cl)
  G <- nrow(sc); n <- length(y); k <- ncol(Xk)
  if (G >= 2 && k > 0) {
    XtXi <- solve(crossprod(Xk))
    V <- (G / (G - 1)) * ((n - 1) / (n - k)) * XtXi %*% crossprod(sc) %*% XtXi
    ep[ok] <- sqrt(diag(V))
  }
  list(coef = b, ep = ep)
}

# A. Transferência (Goodman) e decomposição do voto de Lula no 2º turno
goodman <- function(dd) {
  if (nrow(dd) < 200) return(NULL)
  X <- as.matrix(dd[, X_cols, with = FALSE]); storage.mode(X) <- "double"
  a <- ols_cl(dd$lula2, X, dd$cl); b <- ols_cl(dd$flavio2, X, dd$cl)
  out <- data.table(origem = rotulos, votos_1t = colSums(X),
                    para_lula = a$coef, ep_lula = a$ep,
                    para_flavio = b$coef, ep_flavio = b$ep)
  out[, votos_para_lula := para_lula * votos_1t]
  out[, pct_do_voto_lula_2t := 100 * votos_para_lula / sum(dd$lula2)]
  out[, fora_de_0_1 := para_lula < -0.05 | para_lula > 1.05 |
                       para_flavio < -0.05 | para_flavio > 1.05]
  out[]
}

# Previsão do % de Lula entre os finalistas no 2º turno (coeficientes da UF)
prever <- function(dd) {
  X <- as.matrix(dd[, X_cols, with = FALSE]); storage.mode(X) <- "double"
  ba <- lm.fit(X, dd$lula2)$coefficients;   ba[is.na(ba)] <- 0
  bb <- lm.fit(X, dd$flavio2)$coefficients; bb[is.na(bb)] <- 0
  pa <- pmax((X %*% ba)[, 1], 0.5); pb <- pmax((X %*% bb)[, 1], 0.5)
  pa / (pa + pb)
}

simetria <- function(a, b, rot) data.table(
  conjunto = rot, a_favor_lula = a, a_favor_flavio = b,
  p_binomial = if (a + b > 0) binom.test(a, a + b, 0.5)$p.value else NA_real_)

# Conjuntos usados nas tabelas de simetria e no teste D
conjuntos <- function(d) list(
  "todas"                          = rep(TRUE, nrow(d)),
  "comuns"                         = d$tipo == "comum",
  "comuns, decis 9-10 de esquerda" = d$tipo == "comum" & d$decil_esq >= 9,
  "comuns, decis 1-2 de esquerda"  = d$tipo == "comum" & d$decil_esq <= 2,
  "presidio/socioeducativo"        = d$tipo == "presidio/socioeducativo",
  "aldeia indigena"                = d$tipo == "aldeia indigena",
  "hospital/abrigo"                = d$tipo == "hospital/abrigo",
  "exterior"                       = d$tipo == "exterior")

lim_teto <- function(n) pmax(exc_min_votos, exc_min_frac * n)

# D. Esperado por simulação: % de Lula no 2º turno = previsto + resíduo
#    padronizado sorteado (com reposição) entre seções do mesmo estrato
#    (UF x faixa do % previsto); os votos nos finalistas (n2) e o 1º turno
#    ficam como observados. Assim a simulação reproduz o ganho médio de cada
#    lado e a dispersão real. Ver a LIMITAÇÃO no cabeçalho.
simular_teto <- function(d, B) {
  sd_i <- sqrt(pmax(d$p_hat * (1 - d$p_hat), p_piso * (1 - p_piso)) / d$n2)
  u    <- d$r / sd_i
  # Estratos para o sorteio: UF x faixa (decil) do % previsto de Lula. O
  # tamanho e a assimetria dos desvios mudam com o % previsto (perto de 100%
  # só se pode errar para baixo); sortear dentro da faixa preserva isso.
  # Estratos com menos de min_estrato seções usam só a faixa (Brasil).
  faixa <- cut(d$p_hat, unique(quantile(d$p_hat, 0:n_faixas / n_faixas)),
               include.lowest = TRUE, labels = FALSE)
  estr  <- paste(d$SG_UF, faixa)
  pequeno <- ave(seq_along(estr), estr, FUN = length) < min_estrato
  estr[pequeno] <- paste("BR", faixa[pequeno])
  idx  <- split(seq_len(nrow(d)), estr)
  lim  <- lim_teto(d$n2)
  M    <- do.call(cbind, conjuntos(d))
  obs_l <- colSums(M & d$acima_lula); obs_f <- colSums(M & d$acima_flavio)
  sim <- replicate(B, {
    us <- numeric(nrow(d))
    for (ix in idx) us[ix] <- u[ix][sample.int(length(ix), length(ix), replace = TRUE)]
    l2 <- round(d$n2 * pmin(pmax(d$p_hat + us * sd_i, 0), 1))
    f2 <- d$n2 - l2
    c(colSums(M & (l2 - d$lula1 - d$folga >= lim)),
      colSums(M & (f2 - d$flavio1 - d$folga >= lim)))
  })
  k <- ncol(M)
  sl <- sim[seq_len(k), , drop = FALSE]; sf <- sim[k + seq_len(k), , drop = FALSE]
  mk <- function(lado, obs, s) data.table(
    conjunto = colnames(M), lado = lado, secoes = colSums(M), acima_teto_obs = obs,
    acima_teto_esp = round(rowMeans(s), 1),
    esp_p2.5 = apply(s, 1, quantile, 0.025), esp_p97.5 = apply(s, 1, quantile, 0.975),
    razao_obs_esp = round(obs / pmax(rowMeans(s), 0.5), 3),
    p_excesso = (rowSums(s >= obs) + 1) / (B + 1),
    p_falta   = (rowSums(s <= obs) + 1) / (B + 1))
  out <- rbind(mk("Lula", obs_l, sl), mk("Flávio", obs_f, sf))
  setorder(out, conjunto, lado)
  # Participação de Lula entre as seções acima do teto (Lula / (Lula + Flávio)):
  # compara a observada com a distribuição simulada. Responde se um lado
  # aparece mais do que o outro ALÉM do que o modelo prevê, mesmo quando a
  # simulação erra o total (os erros de dispersão afetam os dois lados).
  sh  <- sl / pmax(sl + sf, 1); sh[(sl + sf) == 0] <- NA
  ob  <- obs_l / pmax(obs_l + obs_f, 1)
  part <- data.table(
    conjunto = colnames(M), acima_lula = obs_l, acima_flavio = obs_f,
    part_lula_obs = round(ifelse(obs_l + obs_f > 0, ob, NA), 4),
    part_lula_esp = round(rowMeans(sh, na.rm = TRUE), 4),
    esp_p2.5  = round(apply(sh, 1, quantile, 0.025, na.rm = TRUE), 4),
    esp_p97.5 = round(apply(sh, 1, quantile, 0.975, na.rm = TRUE), 4),
    simulacoes_validas = rowSums(!is.na(sh)))
  part[, p_bilateral := vapply(seq_len(.N), function(i) {
    s <- sh[i, ]; s <- s[!is.na(s)]
    if (!length(s) || (acima_lula[i] + acima_flavio[i]) == 0) return(NA_real_)
    min(1, 2 * min(sum(s >= ob[i]) + 1, sum(s <= ob[i]) + 1) / (length(s) + 1))
  }, numeric(1))]
  list(contagem = out[], participacao = part[])
}

# Testes B a E (função pura: usada nos dados reais e na validação)
analisar <- function(d, B_sim = 0) {
  d <- copy(d)
  d[, p_hat := prever(.SD), by = SG_UF]
  d[, `:=`(s1 = lula1 / v1, s2 = lula2 / n2, esq_total1 = (lula1 + esq1) / v1)]
  d[, `:=`(ganho_obs_pp  = 100 * (s2 - s1),
           ganho_prev_pp = 100 * (p_hat - s1),
           r = s2 - p_hat)]

  # C. probabilidade por seção (variância mínima p_piso)
  d[, n_mun := .N, by = mun]
  d[, r_c := r - weighted.mean(r, n2), by = mun]
  d[, z0 := r_c / sqrt(pmax(p_hat * (1 - p_hat), p_piso * (1 - p_piso)) / n2)]
  d[n_mun < min_sec, z0 := NA_real_]
  d[, z := z_rob(z0)]
  d[, q := bh(2 * pnorm(-abs(z)))]
  d[, sinal := !is.na(q) & q < alfa_fdr & abs(100 * r_c) >= dif_min_pp]

  # D. teto
  d[, folga := esq1 + outros1 + bn1 + pmax(0L, saldo)]
  d[, `:=`(exc_lula = lula2 - (lula1 + folga), exc_flavio = flavio2 - (flavio1 + folga))]
  d[, `:=`(acima_lula = exc_lula >= lim_teto(n2), acima_flavio = exc_flavio >= lim_teto(n2))]

  # B. ganho ~ reserva de esquerda, efeito fixo de município (só seções comuns)
  dc <- d[tipo == "comum"]
  X <- data.frame(reserva_esquerda_pp = 100 * dc$esq1 / dc$v1,
                  centro_direita_pp   = 100 * dc$outros1 / dc$v1,
                  branco_nulo_1t_pp   = 100 * dc$bn1 / dc$tot1,
                  saldo_comparec_pp   = 100 * dc$saldo / dc$tot1)
  reg <- fe_lm(dc$ganho_obs_pp, X, fe = dc$mun, cl = dc$cl, w = dc$n2)

  # E. decis de força da esquerda no 1º turno (todas as seções definem os
  #    cortes; as contagens de sinalizadas são só das seções comuns)
  qs <- unique(quantile(d$esq_total1, 0:10 / 10))
  d[, decil_esq := cut(esq_total1, qs, include.lowest = TRUE, labels = FALSE)]
  dec <- d[, .(secoes = .N,
               esquerda_1t_pct     = round(100 * weighted.mean(esq_total1, v1), 2),
               lula_1t_pct         = round(100 * weighted.mean(s1, v1), 2),
               reserva_esq_pp      = round(100 * weighted.mean(esq1 / v1, v1), 3),
               centro_direita_pp   = round(100 * weighted.mean(outros1 / v1, v1), 2),
               ganho_obs_pp        = round(weighted.mean(ganho_obs_pp, n2), 3),
               ganho_prev_pp       = round(weighted.mean(ganho_prev_pp, n2), 3),
               residuo_pp          = round(mean(100 * r_c), 3),
               ic95_inf            = round(mean(100 * r_c) - 1.96 * sd(100 * r_c) / sqrt(.N), 3),
               ic95_sup            = round(mean(100 * r_c) + 1.96 * sd(100 * r_c) / sqrt(.N), 3),
               secoes_especiais    = sum(tipo != "comum"),
               sinal_pro_lula_comuns   = sum(sinal & r_c > 0 & tipo == "comum"),
               sinal_pro_flavio_comuns = sum(sinal & r_c < 0 & tipo == "comum"),
               acima_teto_lula_comuns   = sum(acima_lula & tipo == "comum"),
               acima_teto_flavio_comuns = sum(acima_flavio & tipo == "comum")),
           by = decil_esq][order(decil_esq)]

  cj <- conjuntos(d)
  sim <- rbindlist(lapply(names(cj), function(nm) {
    m <- cj[[nm]]
    simetria(sum(d$sinal & d$r_c > 0 & m), sum(d$sinal & d$r_c < 0 & m), nm)
  }))
  sim <- cbind(teste = "C. resíduo extremo (FDR)", sim)

  teto <- if (B_sim > 0) simular_teto(d, B_sim) else NULL

  resumo <- data.table(secoes = nrow(d), secoes_comuns = d[tipo == "comum", .N],
                       secoes_testadas_C = d[!is.na(z), .N],
                       sinalizadas_C = d[sinal == TRUE, .N],
                       sinalizadas_C_comuns = d[sinal == TRUE & tipo == "comum", .N],
                       acima_teto_lula = d[acima_lula == TRUE, .N],
                       acima_teto_flavio = d[acima_flavio == TRUE, .N],
                       cor_ganho_obs_prev = round(cor(d$ganho_obs_pp, d$ganho_prev_pp), 4))
  list(d = d, reg = reg, dec = dec, sim = sim, teto = teto, resumo = resumo)
}

# F. Validação: troca de k votos entre os finalistas (metade para cada lado),
#    só em seções comuns com município de referência
injetar <- function(d, n, frac) {
  d <- copy(d)
  cand <- d[, which(lula2 >= 20 & flavio2 >= 20 & n_mun_ok & tipo == "comum")]
  alvo <- sample(cand, min(n, length(cand)))
  pro  <- runif(length(alvo)) < 0.5
  base <- fifelse(pro, d$flavio2[alvo], d$lula2[alvo])
  k <- as.integer(floor(runif(length(alvo), frac[1], frac[2]) * base))
  d[alvo, `:=`(lula2 = lula2 + fifelse(pro, k, -k), flavio2 = flavio2 + fifelse(pro, -k, k))]
  d[, `:=`(fraude = FALSE, fraude_pro_lula = NA)]
  d[alvo, `:=`(fraude = TRUE, fraude_pro_lula = pro)]
  d
}

# =============================================================================
if (!exists("NAO_EXECUTAR_12")) {
  etapa("N14. Leitura e grupos de candidatos")
  pres <- ler_presidente()
  cands <- unique(pres[NR_TURNO == 1 & !NR_VOTAVEL %in% especiais, .(NR_VOTAVEL, NM_VOTAVEL)])
  cands[, grupo := fcase(NR_VOTAVEL == n_lula, "Lula", NR_VOTAVEL == n_flavio, "Flávio",
                         NR_VOTAVEL %in% esq_outros, "outro de esquerda",
                         default = "outro (centro/direita)")]
  print(cands[order(grupo, NR_VOTAVEL)])
  falta <- setdiff(esq_outros, cands$NR_VOTAVEL)
  if (length(falta)) stop("Números em esq_outros que não estão no 1º turno: ", paste(falta, collapse = ", "))

  d <- montar(pres); rm(pres); invisible(gc())
  message("Seções nos dois turnos (>= ", min_votos, " votos): ", nrow(d))
  print(d[, .N, by = tipo][order(-N)])
  message("UFs com erro-padrão por zona (um só município): ",
          paste(d[cl != mun, unique(SG_UF)], collapse = ", "))

  # Panorama: Brasil e UF
  pan <- function(x) x[, .(secoes = .N,
    lula_1t_pct = 100 * sum(lula1) / sum(v1), esq_outros_1t_pct = 100 * sum(esq1) / sum(v1),
    esquerda_total_1t_pct = 100 * sum(lula1 + esq1) / sum(v1),
    centro_direita_1t_pct = 100 * sum(outros1) / sum(v1),
    flavio_1t_pct = 100 * sum(flavio1) / sum(v1),
    lula_2t_pct = 100 * sum(lula2) / sum(n2))]
  panorama <- rbind(pan(d)[, UF := "BRASIL"],
                    rbindlist(lapply(split(d, by = "SG_UF"), function(x) pan(x)[, UF := x$SG_UF[1]])))
  panorama[, `:=`(ganho_lula_pp = lula_2t_pct - lula_1t_pct,
                  ganho_alem_da_esquerda_pp = lula_2t_pct - esquerda_total_1t_pct)]
  setcolorder(panorama, "UF")
  num_cols <- names(panorama)[vapply(panorama, is.double, logical(1))]
  panorama[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]
  print(panorama[UF == "BRASIL"])

  etapa("N14-A. Transferência de votos (Goodman)")
  tr <- rbind(goodman(d)[, UF := "BRASIL"],
              goodman(d[tipo == "comum"])[, UF := "BRASIL (só seções comuns)"],
              rbindlist(lapply(split(d, by = "SG_UF"), function(x) {
                g <- goodman(x); if (!is.null(g)) g[, UF := x$SG_UF[1]]; g })))
  setcolorder(tr, "UF")
  print(tr[UF == "BRASIL", .(origem, para_lula = round(para_lula, 3), ep_lula = round(ep_lula, 3),
                             para_flavio = round(para_flavio, 3),
                             pct_do_voto_lula_2t = round(pct_do_voto_lula_2t, 2))])
  message("Erro-padrão do DF (deve ser finito): ",
          paste(round(tr[UF == "DF", ep_lula], 4), collapse = " "))

  etapa("N14-B a E. Testes por seção (teste D com ", n_sim_teto, " simulações)")
  r <- analisar(d, B_sim = n_sim_teto)
  message("  B. Ganho de Lula (p.p.) ~ reserva de esquerda e demais fontes (EF de município, seções comuns)")
  print(r$reg)
  message("  C. Resumo e simetria por tipo de local")
  print(r$resumo); print(r$sim)
  message("  D. Acima do teto: observado x esperado por simulação")
  print(r$teto$contagem[, .(conjunto, lado, acima_teto_obs, acima_teto_esp, esp_p2.5, esp_p97.5,
                            razao_obs_esp, p_excesso)])
  message("  D. Participação de Lula entre as seções acima do teto: observada x simulada")
  print(r$teto$participacao)
  message("  E. Por decil de força da esquerda no 1º turno")
  print(r$dec)

  etapa("N14-F. Validação com troca simulada de votos (cópia em memória)")
  d[, n_mun_ok := .N >= min_sec, by = mun]
  val <- rbindlist(lapply(list(c(0.05, 0.15), c(0.15, 0.30)), function(fr) {
    dv <- injetar(d, n_validar, fr)
    rv <- analisar(dv)$d
    data.table(fracao_trocada = sprintf("%.0f-%.0f%% dos votos do lado perdedor", 100 * fr[1], 100 * fr[2]),
               secoes_com_fraude = dv[fraude == TRUE, .N],
               C_encontradas = rv[fraude == TRUE & sinal & sign(r_c) == fifelse(fraude_pro_lula, 1, -1), .N],
               C_sinalizadas_total = rv[sinal == TRUE, .N],
               D_encontradas = rv[fraude == TRUE & fifelse(fraude_pro_lula, acima_lula, acima_flavio), .N],
               D_acima_teto_total = rv[acima_lula | acima_flavio, .N],
               sinalizadas_C_dados_reais = r$resumo$sinalizadas_C,
               acima_teto_dados_reais = r$resumo$acima_teto_lula + r$resumo$acima_teto_flavio)
  }))
  print(val)

  # ---- Saídas ----------------------------------------------------------------
  salvar(cands[order(grupo, NR_VOTAVEL)], "N14_grupos_candidatos.csv")
  salvar(panorama, "N14_panorama.csv")
  salvar(tr, "N14_transferencia.csv")
  salvar(r$reg, "N14_ganho_x_reserva_esquerda.csv")
  salvar(r$dec, "N14_decis_esquerda.csv")
  salvar(r$sim, "N14_simetria.csv")
  salvar(r$teto$contagem, "N14_teto_simulado.csv")
  salvar(r$teto$participacao, "N14_teto_participacao.csv")
  salvar(r$resumo, "N14_resumo.csv")
  salvar(val, "N14_validacao.csv")
  salvar(r$d[, .N, by = tipo][order(-N)], "N14_tipos_de_local.csv")
  cols_sec <- c(sec, "NM_MUNICIPIO", "NM_LOCAL_VOTACAO", "tipo", "tot1", "lula1", "flavio1", "esq1",
                "outros1", "bn1", "tot2", "lula2", "flavio2", "bn2")
  salvar(r$d[sinal == TRUE][order(q), c(cols_sec, "p_hat", "s2", "r_c", "z", "q", "decil_esq"),
                            with = FALSE], "N14_secoes_sinalizadas_C.csv")
  salvar(r$d[acima_lula | acima_flavio][order(-pmax(exc_lula, exc_flavio)),
             c(cols_sec, "exc_lula", "exc_flavio", "decil_esq"), with = FALSE],
         "N14_secoes_acima_teto_D.csv")
  # municípios com várias seções acima do teto (mudança local, não de urna isolada)
  salvar(r$d[acima_lula | acima_flavio, .(secoes_acima = .N, pro_lula = sum(acima_lula),
                                          pro_flavio = sum(acima_flavio)),
             by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)][order(-secoes_acima)],
         "N14_teto_por_municipio.csv")

  png(file.path(pasta_saida, "N14_ganho_lula_por_decil_esquerda.png"), width = 950, height = 600)
  par(mar = c(5, 5, 4, 2))
  dec <- r$dec
  yl <- range(c(dec$ganho_obs_pp, dec$ganho_prev_pp, dec$reserva_esq_pp, 0))
  plot(dec$esquerda_1t_pct, dec$ganho_obs_pp, type = "b", pch = 19, ylim = yl,
       xlab = "Esquerda no 1º turno (Lula + outros de esquerda, % dos válidos, média do decil)",
       ylab = "Ganho de Lula do 1º para o 2º turno (p.p.)",
       main = "Ganho de Lula entre turnos por decil de força da esquerda")
  lines(dec$esquerda_1t_pct, dec$ganho_prev_pp, type = "b", pch = 1, lty = 2)
  lines(dec$esquerda_1t_pct, dec$reserva_esq_pp, type = "b", pch = 4, lty = 3, col = "grey40")
  abline(h = 0, col = "grey70")
  legend("topright", bty = "n", pch = c(19, 1, 4), lty = c(1, 2, 3), col = c("black", "black", "grey40"),
         legend = c("observado", "previsto pelo 1º turno da seção", "reserva de esquerda (outros de esquerda)"))
  dev.off()

  message("\nConcluído. Resultados em resultados/N14_*.csv")
}

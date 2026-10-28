# =============================================================================
# 11_teste_liberacao_biografica.R - Novo teste 13 (N13): liberação biográfica
#                                   com eleitor ausente ("voto do vovô")
#
# Hipótese (van de Graaf, "O mito da urna", 2017, cap. 4): o mesário pode
# desativar a biometria e liberar a urna com o título de um eleitor que não
# compareceu. Se isso acontecesse em escala, cada voto fraudado deixaria
# três marcas na MESMA seção: (1) uma liberação biográfica a mais no BU;
# (2) um eleitor a mais no comparecimento; (3) em geral, um voto a mais no
# fim do dia, quando já se sabe quem não veio. Uma falha do leitor de
# digitais deixa só a marca (1): mais liberações biográficas, sem eleitores
# a mais.
#
# NÃO PRECISA REPROCESSAR os arquivos de urna: usa apenas o que o 09 já
# gravou em tse_2026/urna/saida (secoes_*.csv e horas_*.csv) e o CSV oficial.
#
# Testes:
#   A. Painel 1º x 2º turno, na mesma seção (o perfil fixo da seção se cancela):
#        dV = 100 (V2 - V1) / A,   dB = 100 (B2 - B1) / A
#      V = comparecimento, B = liberações biográficas, A = aptos. Regressão
#      dV ~ dB com efeito fixo de município e erro-padrão por município.
#      O coeficiente é o nº de eleitores a mais por liberação biográfica a
#      mais: ~0 se as liberações vêm de falhas do leitor; ~1 se vêm de
#      eleitores ausentes. Também dL (votos depois das 15h, local) ~ dB.
#   B. Corte transversal por turno: comparecimento e % de votos depois das
#      15h ~ taxa de liberação biográfica, efeito fixo de município.
#   C. Seções extremas: escore combinado das três marcas (z robusto dentro
#      do município), S = (z_B + z_V + z_L) / sqrt(3) > 4, com z_B > 1,
#      z_V > 1 e z_L > 0, no painel (variações dB, dV, dL) e no corte
#      transversal de cada turno; teste de simetria do voto (binomial), como
#      no T8.
#   D. Validação: insere liberações fraudulentas em 300 seções reais do 2º
#      turno (cópia dos dados, em memória), com 10-40% e 40-80% dos ausentes,
#      e mede quantas o escore encontra e quantas seções limpas ele sinaliza.
#
# Saídas: resultados/N13_*.csv
# Pré-requisito: 09 e 10 rodados.
# =============================================================================
source("00_config.R")

s_limite  <- 4       # escore combinado mínimo: (z1 + z2 + z3) / sqrt(3)
min_sec   <- 5       # seções mínimas no município para calcular o z
hora_tarde <- 15     # "fim do dia": votos a partir das 15h (hora local da urna)
n_validar <- 300     # seções com fraude simulada na validação
set.seed(2026)

pasta_out <- file.path(pasta_base, "urna", "saida")
ler_saida <- function(prefixo) {
  arqs <- list.files(pasta_out, sprintf("^%s_t[12]_.*\\.csv$", prefixo), full.names = TRUE)
  if (!length(arqs)) return(NULL)
  rbindlist(lapply(arqs, fread, sep = ";", encoding = "UTF-8",
                   colClasses = list(character = c("uf", "cd_mun", "zona", "secao"))), fill = TRUE)
}
para_chave <- function(d) {
  d[, `:=`(NR_TURNO = as.integer(turno), SG_UF = toupper(uf), CD_MUNICIPIO = as.integer(cd_mun),
           NR_ZONA = as.integer(zona), NR_SECAO = as.integer(secao))]
  d
}
num <- function(v) suppressWarnings(as.numeric(v))
zr  <- function(x, piso) (x - median(x, na.rm = TRUE)) / pmax(mad(x, na.rm = TRUE), piso)

# ---- Montagem: uma linha por seção e turno ------------------------------------
montar <- function(secs, horas, lg) {
  s <- secs[origem_bu == "bu" & origem_log == "log",
            c(chave, "aptos_bu", "comparecimento_bu", "habilitados_biografia"), with = FALSE]
  for (cc in c("aptos_bu", "comparecimento_bu", "habilitados_biografia")) set(s, j = cc, value = num(s[[cc]]))
  setnames(s, c("aptos_bu", "comparecimento_bu", "habilitados_biografia"), c("A", "V", "B"))
  s <- s[A > 0 & V > 0 & !is.na(B)]
  h <- horas[, .(L = sum(votos[num(hora_local) >= hora_tarde], na.rm = TRUE),
                 T = sum(votos, na.rm = TRUE)), by = chave]
  d <- merge(s, h, by = chave)
  d <- merge(d, lg[, c(chave, "lula", "flavio", "mun"), with = FALSE], by = chave)
  d[T > 0]
}

# ---- Análise (função pura: usada nos dados reais e na validação) --------------
analisar <- function(d) {
  d <- copy(d)
  d[, `:=`(tx_bio = 100 * B / V, comparec = 100 * V / A, tarde = 100 * L / T,
           s = fifelse(lula + flavio > 0, lula / (lula + flavio), NA_real_))]
  d[, n_mun := .N, by = .(NR_TURNO, mun)]
  d[n_mun >= min_sec, `:=`(z_bio = zr(tx_bio, 0.5), z_comp = zr(comparec, 0.5), z_tarde = zr(tarde, 0.5),
                           d_s = s - sum(lula) / sum(lula + flavio)), by = .(NR_TURNO, mun)]

  # B. corte transversal por turno
  rB <- rbindlist(lapply(split(d[lula + flavio > 30], by = "NR_TURNO"), function(x) rbind(
    fe_lm(x$comparec, data.frame(tx_bio_pp = x$tx_bio), fe = x$mun, cl = x$mun, w = x$A)[, desfecho := "comparecimento_pp"],
    fe_lm(x$tarde, data.frame(tx_bio_pp = x$tx_bio), fe = x$mun, cl = x$mun, w = x$T)[, desfecho := "votos_apos_15h_pp"]
  )[, NR_TURNO := x$NR_TURNO[1]]))

  # A. painel 1º x 2º turno
  p <- merge(d[NR_TURNO == 1, c(sec, "mun", "A", "V", "B", "L", "T", "s"), with = FALSE],
             d[NR_TURNO == 2, c(sec, "A", "V", "B", "L", "T", "s"), with = FALSE],
             by = sec, suffixes = c("1", "2"))
  p <- p[abs(A2 - A1) <= 0.05 * A2]          # mesma seção, mesmo eleitorado (aptos quase iguais)
  p[, `:=`(dV = 100 * (V2 - V1) / A2, dB = 100 * (B2 - B1) / A2,
           dL = 100 * (L2 / T2 - L1 / T1), ds = s2 - s1)]
  rA <- rbind(fe_lm(p$dV, data.frame(dB_pp = p$dB), fe = p$mun, cl = p$mun)[, desfecho := "dV_comparecimento_pp"],
              fe_lm(p$dL, data.frame(dB_pp = p$dB), fe = p$mun, cl = p$mun)[, desfecho := "dL_votos_apos_15h_pp"])
  p[, n_mun := .N, by = mun]
  p[n_mun >= min_sec, `:=`(z_dB = zr(dB, 0.5), z_dV = zr(dV, 0.5), z_dL = zr(dL, 0.5),
                           d_ds = ds - median(ds, na.rm = TRUE)), by = mun]

  # C. seções extremas e simetria
  d[, S := (z_bio + z_comp + z_tarde) / sqrt(3)]
  p[, S := (z_dB + z_dV + z_dL) / sqrt(3)]
  cs <- d[!is.na(S) & z_bio > 1 & z_comp > 1 & z_tarde > 0 & S > s_limite]
  pa <- p[!is.na(S) & z_dB > 1 & z_dV > 1 & z_dL > 0 & S > s_limite]
  simetria <- function(x, rot) {
    x <- x[!is.na(x) & x != 0]
    if (!length(x)) return(data.table(conjunto = rot, secoes = 0L, a_favor_lula = 0L, a_favor_flavio = 0L, p_binomial = NA_real_))
    data.table(conjunto = rot, secoes = length(x), a_favor_lula = sum(x > 0), a_favor_flavio = sum(x < 0),
               p_binomial = binom.test(sum(x > 0), length(x), 0.5)$p.value)
  }
  sim <- rbind(simetria(cs[NR_TURNO == 1, d_s], "corte_turno1"), simetria(cs[NR_TURNO == 2, d_s], "corte_turno2"),
               simetria(pa$d_ds, "painel"))
  list(d = d, p = p, rA = rA, rB = rB, cs = cs, pa = pa, sim = sim,
       resumo = data.table(secoes_turno1 = d[NR_TURNO == 1, .N], secoes_turno2 = d[NR_TURNO == 2, .N],
                           secoes_painel = nrow(p),
                           extremas_3_marcas_t1 = cs[NR_TURNO == 1, .N], extremas_3_marcas_t2 = cs[NR_TURNO == 2, .N],
                           extremas_painel = nrow(pa)))
}

# ---- D. Validação: fraude simulada em cópia dos dados ------------------------
injetar <- function(d, n, frac = c(0.10, 0.40), turno = 2L) {
  d <- copy(d)
  cand <- d[NR_TURNO == turno & (A - V) >= 20, which = TRUE]
  alvo <- sample(cand, min(n, length(cand)))
  k <- floor(runif(length(alvo), frac[1], frac[2]) * (d$A[alvo] - d$V[alvo]))   # fração dos ausentes
  pro_lula <- runif(length(alvo)) < 0.5
  d[alvo, `:=`(V = V + k, B = B + k, L = L + k, T = T + k,
               lula = lula + fifelse(pro_lula, k, 0), flavio = flavio + fifelse(pro_lula, 0, k))]
  d[, fraude := FALSE][alvo, fraude := TRUE]
  d
}

# =============================================================================
if (!exists("NAO_EXECUTAR_11")) {
  etapa("N13. Leitura (sem reprocessar os arquivos de urna)")
  secs <- ler_saida("secoes"); horas <- ler_saida("horas")
  if (is.null(secs) || is.null(horas)) stop("Faltam secoes_*.csv ou horas_*.csv em ", pasta_out, ". Rode o 09 antes.")
  secs <- para_chave(secs); horas <- para_chave(horas)
  secs <- unique(secs, by = chave)
  lg <- largo_finalistas(ler_presidente())
  lg[, mun := paste(SG_UF, CD_MUNICIPIO)]
  d <- montar(secs, horas, lg)
  print(d[, .(secoes = .N, liberacao_biografica_pct = round(100 * sum(B) / sum(V), 2),
              votos_apos_15h_pct = round(100 * sum(L) / sum(T), 2)), by = NR_TURNO])

  etapa("N13. Testes")
  r <- analisar(d)
  message("  A. Painel 1º x 2º turno (mesma seção): variação de eleitores por liberação biográfica a mais")
  print(r$rA)
  message("  B. Corte transversal por turno")
  print(r$rB)
  message("  C. Seções extremas e simetria do voto")
  print(r$resumo); print(r$sim)

  etapa("N13. Validação com fraude simulada (cópia em memória)")
  val <- rbindlist(lapply(list(c(0.10, 0.40), c(0.40, 0.80)), function(fr) {
    dv <- injetar(d, n_validar, fr)
    rv <- analisar(dv)
    ac <- merge(rv$cs[NR_TURNO == 2, chave, with = FALSE], dv[fraude == TRUE, chave, with = FALSE], by = chave)
    ap <- merge(rv$pa[, sec, with = FALSE], dv[fraude == TRUE, sec, with = FALSE], by = sec)
    data.table(fracao_ausentes = sprintf("%.0f-%.0f%%", 100 * fr[1], 100 * fr[2]),
               secoes_com_fraude = dv[fraude == TRUE, .N],
               encontradas_painel = nrow(ap), sinalizadas_painel = nrow(rv$pa),
               encontradas_corte_t2 = nrow(ac), sinalizadas_corte_t2 = rv$cs[NR_TURNO == 2, .N],
               coef_painel_dados_reais = r$rA[desfecho == "dV_comparecimento_pp", coef],
               coef_painel_com_fraude = rv$rA[desfecho == "dV_comparecimento_pp", coef],
               ep_painel_com_fraude = rv$rA[desfecho == "dV_comparecimento_pp", ep])
  }))
  print(val)

  salvar(r$resumo, "N13_resumo.csv")
  salvar(r$rA, "N13_painel_dV_dB.csv")
  salvar(r$rB, "N13_corte_transversal.csv")
  salvar(r$sim, "N13_simetria.csv")
  salvar(val, "N13_validacao.csv")
  salvar(r$cs[order(-S), c(chave, "A", "V", "B", "L", "T", "tx_bio", "comparec", "tarde",
                            "z_bio", "z_comp", "z_tarde", "S", "lula", "flavio"), with = FALSE],
         "N13_secoes_extremas_3_marcas.csv")
  salvar(r$pa[order(-S), c(sec, "A2", "V1", "V2", "B1", "B2", "dV", "dB", "dL", "z_dB", "z_dV", "z_dL", "S", "ds"),
              with = FALSE], "N13_secoes_extremas_painel.csv")
  message("\nConcluído. Resultados em resultados/N13_*.csv")
}

# =============================================================================
# 05_cruzamento_comparecimento_perfil.R
# T8. Cruzamento: variação de comparecimento x mudança de perfil de voto
#     entre o 1º e o 2º turno - Presidente 2026 - todas as seções
#
# Pré-requisito: 01 e 02 rodados com sucesso.
#
# Pergunta: nas seções em que o comparecimento subiu (ou caiu) mais do que
# nas outras seções do mesmo município, o voto mudou de lado de forma
# desproporcional? Inflar uma seção com votos de um só candidato deixa
# exatamente essa marca: mais votos + perfil deslocado para um lado
# (Klimek et al., 2012).
#
# Medidas por seção (comparando 1º e 2º turno da MESMA seção):
#   var_comparec = log(votos 2º turno / votos 1º turno)   (total, com brancos e nulos)
#   p1 = A / (A + B) no 1º turno  (A e B = os dois finalistas)
#   p2 = A / (A + B) no 2º turno
#   mudanca_perfil = p2 - p1      (em pontos percentuais)
#   perfil_residual = p2 - p2 previsto pelo 1º turno da seção (mudança NÃO
#                     explicada pela composição do voto no 1º turno)
# Ambas centradas na mediana do município (o que importa é a seção destoar
# das vizinhas, não do país).
#
# Saídas (pasta resultados):
#   T8_decis.csv              mudança de perfil média por decil de variação de comparecimento
#   T8_correlacao.csv         correlação (Spearman) dentro do município, Brasil e por UF
#   T8_transferencia.csv      regressão ecológica de transferência de votos (Goodman), por UF
#   T8_secoes_sinalizadas.csv seções com comparecimento E perfil extremos ao mesmo tempo
#   T8_decis.png / T8_dispersao.png
# =============================================================================

source("00_config.R")

z_comp_min  <- 4      # |z| robusto mínimo da variação de comparecimento
z_perf_min  <- 2      # |z| robusto mínimo da mudança de perfil não explicada
min_votos   <- 30     # seções com menos votos que isso em algum turno ficam de fora

etapa("Leitura e teste de carga")
pres <- ler_presidente()

# ---- 2. Finalistas (A = mais votado no 2º turno) ---------------------------------
fin <- pres[NR_TURNO == 2 & !(NR_VOTAVEL %in% c(95L, 96L, 97L)),
            .(v = sum(QT_VOTOS)), by = .(NR_VOTAVEL, NM_VOTAVEL)][order(-v)]
if (nrow(fin) != 2) stop("Esperava 2 finalistas no 2º turno; achei ", nrow(fin))
nA <- fin$NR_VOTAVEL[1]; nB <- fin$NR_VOTAVEL[2]
nomeA <- fin$NM_VOTAVEL[1]; nomeB <- fin$NM_VOTAVEL[2]
message("A = ", nomeA, " | B = ", nomeB)

# ---- 3. Uma linha por seção, com os dois turnos ---------------------------------
resumo_turno <- function(tr) {
  pres[NR_TURNO == tr, .(
    total = sum(QT_VOTOS),
    vA    = sum(QT_VOTOS[NR_VOTAVEL == nA]),
    vB    = sum(QT_VOTOS[NR_VOTAVEL == nB]),
    vBN   = sum(QT_VOTOS[NR_VOTAVEL %in% c(95L, 96L, 97L)]),
    local = NM_LOCAL_VOTACAO[1],
    NM_MUNICIPIO = NM_MUNICIPIO[1]
  ), by = sec]
}
t1 <- resumo_turno(1L); t2 <- resumo_turno(2L)
t1[, vOutros := total - vA - vB - vBN]
d <- merge(t1, t2[, c(sec, "total", "vA", "vB", "vBN"), with = FALSE],
           by = sec, suffixes = c("1", "2"))
setnames(d, c("vOutros"), c("vOutros1"))
d <- d[total1 >= min_votos & total2 >= min_votos & (vA1 + vB1) > 0 & (vA2 + vB2) > 0]
message("Seções nos dois turnos (com >= ", min_votos, " votos): ", nrow(d))

d[, var_comparec   := log(total2 / total1)]
d[, p1             := vA1 / (vA1 + vB1)]
d[, p2             := vA2 / (vA2 + vB2)]
d[, mudanca_perfil := 100 * (p2 - p1)]          # pontos percentuais, a favor de A

# Parte da mudança de perfil é esperada: seções onde terceiros tiveram muitos
# votos mudam mais no 2º turno. Por isso, além da mudança bruta, calculamos a
# mudança NÃO EXPLICADA pela composição do 1º turno da própria seção:
# previsão de A e B no 2º turno a partir dos votos do 1º turno (A, B, outros,
# brancos/nulos), estimada por UF, sem usar o comparecimento.
prever <- function(dd) {
  fa <- lm(vA2 ~ 0 + vA1 + vB1 + vOutros1 + vBN1, data = dd)
  fb <- lm(vB2 ~ 0 + vA1 + vB1 + vOutros1 + vBN1, data = dd)
  pa <- pmax(predict(fa, dd), 0.5); pb <- pmax(predict(fb, dd), 0.5)
  pa / (pa + pb)
}
d[, p2_previsto := prever(.SD), by = SG_UF]
d[, perfil_residual := 100 * (p2 - p2_previsto)]    # p.p. a favor de A além do previsto

# Centrar no município (mediana) - a seção comparada com as vizinhas
d[, `:=`(n_mun = .N,
         comparec_c = var_comparec - median(var_comparec),
         perfil_c   = mudanca_perfil - median(mudanca_perfil),
         residual_c = perfil_residual - median(perfil_residual)),
  by = .(SG_UF, CD_MUNICIPIO)]
d <- d[n_mun >= 3]                                # precisa de vizinhas para comparar

# ---- 4. Decis de variação de comparecimento -------------------------------------
d[, decil := cut(comparec_c, quantile(comparec_c, 0:10 / 10), include.lowest = TRUE,
                 labels = 1:10)]
decis <- d[, .(secoes = .N,
               var_comparec_media_pct = round(100 * (exp(mean(comparec_c)) - 1), 2),
               mudanca_perfil_media_pp = round(mean(perfil_c), 3),
               nao_explicada_media_pp  = round(mean(residual_c), 3),
               ic95_inf = round(mean(residual_c) - 1.96 * sd(residual_c) / sqrt(.N), 3),
               ic95_sup = round(mean(residual_c) + 1.96 * sd(residual_c) / sqrt(.N), 3)),
           by = decil][order(decil)]
decis[, candidato_A := nomeA]
print(decis)
salvar(decis, "T8_decis.csv")

# ---- 5. Correlação dentro do município (Brasil e por UF) ------------------------
cor_s <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))
correl <- rbind(
  d[, .(UF = "BRASIL", secoes = .N, spearman = round(cor_s(comparec_c, residual_c), 4),
        spearman_bruto = round(cor_s(comparec_c, perfil_c), 4))],
  d[, .(secoes = .N, spearman = round(cor_s(comparec_c, residual_c), 4),
        spearman_bruto = round(cor_s(comparec_c, perfil_c), 4)),
    by = .(UF = SG_UF)][order(-abs(spearman))]
)
correl[, leitura := fifelse(spearman > 0,
                            paste0("comparecimento maior acompanha deslocamento a favor de ", nomeA),
                            paste0("comparecimento maior acompanha deslocamento a favor de ", nomeB))]
print(correl)
salvar(correl, "T8_correlacao.csv")

# ---- 6. Transferência de votos (regressão ecológica de Goodman) -----------------
# votos do candidato no 2º turno explicados pela composição do 1º turno
# e pelo eleitor a mais (ou a menos) que apareceu. Coeficientes = taxas de
# transferência; esperado entre 0 e 1. Valor fora disso = modelo não se
# ajusta naquela UF (não é prova de nada, mas pede atenção).
d[, saldo := total2 - total1]
transf <- function(dd) {
  if (nrow(dd) < 50) return(NULL)
  fA <- tryCatch(coef(lm(vA2 ~ 0 + vA1 + vB1 + vOutros1 + vBN1 + saldo, data = dd)),
                 error = function(e) NULL)
  fB <- tryCatch(coef(lm(vB2 ~ 0 + vA1 + vB1 + vOutros1 + vBN1 + saldo, data = dd)),
                 error = function(e) NULL)
  if (is.null(fA) || is.null(fB)) return(NULL)
  data.table(origem = c(paste("votou em", nomeA), paste("votou em", nomeB),
                        "votou em outros", "branco/nulo", "eleitor a mais no 2º turno"),
             para_A = round(fA, 3), para_B = round(fB, 3))
}
tr <- rbind(transf(d)[, UF := "BRASIL"],
            rbindlist(lapply(split(d, by = "SG_UF"), function(x)
              transf(x)[, UF := x$SG_UF[1]])), fill = TRUE)
setcolorder(tr, c("UF", "origem", "para_A", "para_B"))
tr[, fora_de_0_1 := para_A < -0.05 | para_A > 1.05 | para_B < -0.05 | para_B > 1.05]
print(tr[UF == "BRASIL"])
salvar(tr, "T8_transferencia.csv")

# ---- 7. Seções extremas nas DUAS medidas ao mesmo tempo -------------------------
d[, `:=`(z_comparec = z_rob(comparec_c), z_perfil = z_rob(residual_c))]
sinal <- d[abs(z_comparec) >= z_comp_min & abs(z_perfil) >= z_perf_min,
           .(SG_UF, NM_MUNICIPIO, CD_MUNICIPIO, NR_ZONA, NR_SECAO, local,
             votos_1t = total1, votos_2t = total2,
             pctA_1t = round(100 * p1, 1), pctA_2t = round(100 * p2, 1),
             pctA_2t_previsto = round(100 * p2_previsto, 1),
             z_comparec = round(z_comparec, 1), z_perfil = round(z_perfil, 1))]
sinal[, padrao := fifelse(z_comparec > 0 & z_perfil > 0, paste("mais votos, a favor de", nomeA),
                  fifelse(z_comparec > 0 & z_perfil < 0, paste("mais votos, a favor de", nomeB),
                  fifelse(z_comparec < 0 & z_perfil > 0, paste("menos votos, a favor de", nomeA),
                                                         paste("menos votos, a favor de", nomeB))))]
setorder(sinal, -z_comparec)
message("Seções extremas nas duas medidas: ", nrow(sinal))
print(sinal[, .N, by = padrao])
salvar(sinal, "T8_secoes_sinalizadas.csv")

# Teste de simetria: se não há manipulação a favor de um lado, entre as seções
# com MAIS votos e perfil extremo, os dois sentidos devem ser parecidos.
mais <- sinal[z_comparec > 0]
if (nrow(mais) > 0) {
  bt <- binom.test(sum(mais$z_perfil > 0), nrow(mais), 0.5)
  message(sprintf("Simetria (seções com mais votos): %d a favor de A, %d a favor de B, p = %.3g",
                  sum(mais$z_perfil > 0), sum(mais$z_perfil < 0), bt$p.value))
  salvar(data.table(secoes_mais_votos_extremas = nrow(mais),
                    a_favor_A = sum(mais$z_perfil > 0), a_favor_B = sum(mais$z_perfil < 0),
                    p_binomial = bt$p.value), "T8_simetria.csv")
}

# ---- 8. Gráficos -------------------------------------------------------------
png(file.path(pasta_saida, "T8_decis.png"), width = 900, height = 550)
par(mar = c(5, 5, 4, 2))
plot(decis$var_comparec_media_pct, decis$nao_explicada_media_pp, type = "b", pch = 19,
     ylim = range(c(decis$ic95_inf, decis$ic95_sup, 0)),
     xlab = "Variação de comparecimento vs. município (%, média do decil)",
     ylab = paste("Mudança não explicada a favor de", nomeA, "(p.p.)"),
     main = "Mudança de perfil por decil de variação de comparecimento")
arrows(decis$var_comparec_media_pct, decis$ic95_inf, decis$var_comparec_media_pct,
       decis$ic95_sup, angle = 90, code = 3, length = 0.04)
abline(h = 0, lty = 2, col = "grey50")
dev.off()

png(file.path(pasta_saida, "T8_dispersao.png"), width = 750, height = 700)
smoothScatter(100 * (exp(d$comparec_c) - 1), d$residual_c, nrpoints = 0,
              xlim = c(-60, 60), ylim = c(-30, 30),
              xlab = "Variação de comparecimento vs. município (%)",
              ylab = paste("Mudança não explicada vs. município (p.p., + =", nomeA, ")"))
abline(h = 0, v = 0, lty = 2, col = "grey50")
dev.off()

message("Concluído. Resultados em: ", normalizePath(pasta_saida))
message("Próximo passo: 06_baixar_dados_abertos_extra.R (novos testes)")

# =============================================================================
# teste_11_simulado.R - Testa o 11_teste_liberacao_biografica.R em dados
# simulados (não precisa dos dados do TSE). Rodar de dentro da pasta scripts/:
#   source("../testes/teste_11_simulado.R")
# Cenário: 200 municípios x 30 seções, 2 turnos, 150 leitores de digital com
# defeito por turno (muitas liberações biográficas sem eleitores a mais).
# Esperado nos dados limpos: inclinação dV ~ dB perto de 0 e poucas seções
# sinalizadas; com a fraude simulada, inclinação maior e várias encontradas.
# =============================================================================
NAO_EXECUTAR_11 <- TRUE
source("11_teste_liberacao_biografica.R")
set.seed(1)
base <- CJ(m = 1:200, sx = 1:30)
base[, `:=`(SG_UF = "XX", CD_MUNICIPIO = m, NR_ZONA = 1L, NR_SECAO = sx, A = rpois(.N, 300),
            pm = plogis(rnorm(1, 1.4, .3)), pl = runif(1, .3, .7)), by = m]
base[, pm := plogis(qlogis(pm) + rnorm(.N, 0, .2))]
simular_turno <- function(t) {
  x <- copy(base); x[, NR_TURNO := t]
  x[, V := rbinom(.N, A, pmin(.98, pm + rnorm(.N, 0, .02)))]
  x[, B := rbinom(.N, V, 0.06)]
  falha <- sample(nrow(x), 150); x[falha, B := rbinom(.N, V, 0.6)]
  x[, `:=`(T = V, L = rbinom(.N, V, .15))]
  x[, lula := rbinom(.N, floor(.95 * V), pl)][, flavio := floor(.95 * V) - lula]
  x[, mun := paste(SG_UF, CD_MUNICIPIO)]
  x
}
d <- rbind(simular_turno(1L), simular_turno(2L))
r <- analisar(d)
cat("\nDados limpos:\n"); print(r$rA[, .(desfecho, coef, ep, p)]); print(r$resumo)
for (fr in list(c(.10, .40), c(.40, .80))) {
  dv <- injetar(d, 300, fr); rv <- analisar(dv)
  achou <- merge(rv$pa[, sec, with = FALSE], dv[fraude == TRUE, sec, with = FALSE], by = sec)
  cat(sprintf("\nFraude em 300 seções (%.0f-%.0f%% dos ausentes): painel encontrou %d (sinalizou %d); inclinação %.3f (ep %.3f)\n",
              100 * fr[1], 100 * fr[2], nrow(achou), nrow(rv$pa),
              rv$rA[desfecho == "dV_comparecimento_pp", coef], rv$rA[desfecho == "dV_comparecimento_pp", ep]))
}

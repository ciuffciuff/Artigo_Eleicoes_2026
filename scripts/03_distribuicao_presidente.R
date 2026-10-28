# =============================================================================
# 03_distribuicao_presidente.R - Distribuição dos votos para Presidente
#                                em cada seção eleitoral, 1º e 2º turno
# Pré-requisito: 01 e 02 rodados com sucesso.
# Saídas em resultados/: 03_presidente_por_secao_longo.csv,
#   03_presidente_por_secao_largo.csv, 03_resumo_distribuicao.csv,
#   03_histograma_2turno.png
# =============================================================================
source("00_config.R")

etapa("Leitura e teste de carga")
pres <- ler_presidente()
print(unique(pres[, .(NR_VOTAVEL, NM_VOTAVEL)])[order(NR_VOTAVEL)])

# ---- 2. Votos e percentuais por seção ---------------------------------------
# Códigos do TSE: 95 = Branco, 96 = Nulo, 97 = Anulado (apuração em separado)
pres[, tipo := fifelse(NR_VOTAVEL %in% especiais, "nao_valido", "valido")]

# Uma seção é identificada por UF + município + zona + seção (+ turno)
chave_d <- c(chave, "NM_MUNICIPIO")   # inclui o nome do município na saída

por_secao <- pres[, .(votos = sum(QT_VOTOS)),
                  by = c(chave_d, "NR_VOTAVEL", "NM_VOTAVEL", "tipo")]

# O arquivo do TSE não traz linhas com 0 votos. Completa com zeros para que
# toda seção tenha todos os candidatos do turno (senão a distribuição fica
# distorcida - as seções com 0 votos sumiriam).
secs  <- unique(por_secao[, ..chave_d])
votav <- unique(por_secao[, .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL, tipo)])
grade <- merge(secs, votav, by = "NR_TURNO", allow.cartesian = TRUE)
por_secao <- merge(grade, por_secao,
                   by = c(chave_d, "NR_VOTAVEL", "NM_VOTAVEL", "tipo"), all.x = TRUE)
por_secao[is.na(votos), votos := 0L]
rm(secs, votav, grade)

# Total da seção (todos os votos) e total de votos válidos
por_secao[, total_secao   := sum(votos), by = chave_d]
por_secao[, validos_secao := sum(votos[tipo == "valido"]), by = chave_d]

# % sobre o total e % sobre válidos (critério oficial de resultado)
por_secao[, pct_total   := round(100 * votos / total_secao, 2)]
por_secao[, pct_validos := fifelse(tipo == "valido" & validos_secao > 0,
                                   round(100 * votos / validos_secao, 2),
                                   NA_real_)]

setorderv(por_secao, c(chave_d, "votos"), order = c(rep(1, length(chave_d)), -1))

# ---- 3. Formato largo: uma linha por seção, uma coluna por candidato --------
largo <- dcast(por_secao,
               as.formula(paste(paste(chave_d, collapse = " + "),
                                "+ total_secao + validos_secao ~ NM_VOTAVEL")),
               value.var = "votos", fill = 0)

# ---- 4. Salvar ---------------------------------------------------------------
salvar(por_secao, "03_presidente_por_secao_longo.csv")
salvar(largo, "03_presidente_por_secao_largo.csv")
# bom = TRUE faz o Excel abrir os acentos corretamente

# ---- 5. Como os percentuais se distribuem entre as seções -------------------
# Resumo (mínimo, quartis, mediana, média, máximo) do % de válidos
# de cada candidato, por turno
resumo <- por_secao[tipo == "valido",
                    .(secoes  = .N,
                      minimo  = min(pct_validos, na.rm = TRUE),
                      q1      = quantile(pct_validos, 0.25, na.rm = TRUE),
                      mediana = median(pct_validos, na.rm = TRUE),
                      media   = round(mean(pct_validos, na.rm = TRUE), 2),
                      q3      = quantile(pct_validos, 0.75, na.rm = TRUE),
                      maximo  = max(pct_validos, na.rm = TRUE)),
                    by = .(NR_TURNO, NM_VOTAVEL)][order(NR_TURNO, -mediana)]
print(resumo)
salvar(resumo, "03_resumo_distribuicao.csv")

# Histograma do 2º turno: um gráfico por candidato
seg <- por_secao[NR_TURNO == 2 & tipo == "valido"]
cands <- unique(seg$NM_VOTAVEL)
png(file.path(pasta_saida, "03_histograma_2turno.png"), width = 500 * length(cands), height = 450)
par(mfrow = c(1, length(cands)))
for (cand in cands) {
  hist(seg[NM_VOTAVEL == cand, pct_validos], breaks = 50,
       main = cand, xlab = "% dos votos válidos na seção",
       ylab = "Nº de seções", col = "grey70", border = "white")
}
dev.off()
message("Concluído. Próximo passo: 04_testes_forenses.R")

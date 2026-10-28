# =============================================================================
# classificar_locais_t6.R - Classificação aproximada, por palavras no nome do
# local de votação, das seções com mudança extrema de comparecimento (T6).
# Entrada: resultados/T6_comparecimento_sinalizadas.csv (gerado pelo 04)
# Saída:   resultados/T6_comparecimento_por_tipo_local.csv e o resumo no console
# Uso: rode na pasta scripts/ (mesma pasta de resultados/ gerada pelo 04)
# A classificação é APROXIMADA: depende das palavras abaixo. Confirme no
# cadastro oficial de locais antes de citar casos individuais.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
arq <- if (file.exists("resultados/T6_comparecimento_sinalizadas.csv")) "resultados/T6_comparecimento_sinalizadas.csv" else
  "../resultados/T6_comparecimento_sinalizadas.csv"
c6 <- fread(arq, encoding = "UTF-8")
sem_acento <- function(x) toupper(chartr("ÁÀÂÃÉÊÍÓÔÕÚÇáàâãéêíóôõúç", "AAAAEEIOOOUCaaaaeeiooouc", x))
pal <- list(
  prisao   = "PRESID|PENITENC|PRISION|CADEIA|SOCIOEDUC|DETENC|CDP|CUSTODIA|PENAL|APAC|INTERNACAO|RESSOCIALIZ|CASE |CENTRO DE ATENDIMENTO|FEBEM|FUNDACAO CASA|CRIMINAL|CARCER|PRESO",
  aldeia   = "ALDEIA|INDIGENA|TERRA IND|POLO BASE",
  hospital = "HOSPITAL|ASILO|ABRIGO|CASA DE REPOUSO|ILPI|LAR |CLINICA|SANATORIO|IDOSO")
c6[, nm := sem_acento(NM_LOCAL_VOTACAO)]
c6[, tipo := fcase(grepl(pal$prisao, nm), "presidio/socioeducativo",
                   grepl(pal$aldeia, nm), "aldeia indigena",
                   grepl(pal$hospital, nm), "hospital/abrigo",
                   SG_UF == "ZZ", "exterior",
                   default = "outro")]
res <- c6[, .(secoes = .N, perderam_eleitores = sum(votos_2turno < votos_1turno),
              ganharam_eleitores = sum(votos_2turno > votos_1turno)), by = tipo][order(-secoes)]
print(res)
fwrite(c6[, !"nm"], sub("T6_comparecimento_sinalizadas", "T6_comparecimento_por_tipo_local", arq), sep = ";", bom = TRUE)

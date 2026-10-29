# =============================================================================
# classificar_locais_t6.R - Classificação aproximada, por palavras no nome do
# local de votação, das seções com mudança extrema de comparecimento (T6).
# Entrada: resultados/T6_comparecimento_sinalizadas.csv (gerado pelo 04)
# Saída:   resultados/T6_comparecimento_por_tipo_local.csv e o resumo no console
# Uso: rode na pasta scripts/ (mesma pasta de resultados/ gerada pelo 04)
# A classificação é APROXIMADA: depende das palavras abaixo. Confirme no
# cadastro oficial de locais antes de citar casos individuais.
#
# Correção (29/10/2026): na versão anterior "LAR " pegava "ESCOLAR "
# (escolas viravam hospital/abrigo), "PRESID" pegava "PRESIDENTE" e
# "CENTRO DE ATENDIMENTO" pegava os CAIC (escolas). Agora as palavras curtas
# precisam ser palavras inteiras. Os mesmos padrões estão no script 12 (N14).
# Nas posições de letra acentuada o padrão usa "..?" (com ou sem acento).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
arq <- if (file.exists("resultados/T6_comparecimento_sinalizadas.csv")) "resultados/T6_comparecimento_sinalizadas.csv" else
  "../resultados/T6_comparecimento_sinalizadas.csv"
c6 <- fread(arq, encoding = "UTF-8")
ini <- "(^|[^A-Z])"; fim <- "([^A-Z]|$)"
pal <- list(
  prisao   = paste0("PRES..?DIO|PENITENC|PRISION|CADEIA P..?BLICA|SOCIOEDUC|DETEN..?..?O|",
                    ini, "CDP|CUST..?DIA|", ini, "PENAL", fim, "|", ini, "APAC", fim, "|INTERNA..?..?O|",
                    "RESSOCIALIZ|", ini, "CASE", fim, "|FEBEM|FUNDA..?..?O CASA|CRIMINAL|CARCER|",
                    ini, "PRESOS?", fim),
  aldeia   = "ALDEIA|IND..?GENA|TERRA IND|POLO BASE",
  hospital = paste0("HOSPITAL|", ini, "ASILO|", ini, "ABRIGO|CASA DE REPOUSO|", ini, "ILPI", fim, "|",
                    ini, "LAR", fim, "|CL..?NICA|SANAT..?RIO|IDOSO"))
c6[, nm := toupper(fifelse(is.na(NM_LOCAL_VOTACAO), "", NM_LOCAL_VOTACAO))]
tem <- function(p) grepl(p, c6$nm, useBytes = TRUE)
c6[, tipo := fcase(tem(pal$prisao), "presidio/socioeducativo",
                   tem(pal$aldeia), "aldeia indigena",
                   tem(pal$hospital), "hospital/abrigo",
                   SG_UF == "ZZ", "exterior",
                   default = "outro")]
res <- c6[, .(secoes = .N, perderam_eleitores = sum(votos_2turno < votos_1turno),
              ganharam_eleitores = sum(votos_2turno > votos_1turno)), by = tipo][order(-secoes)]
print(res)
fwrite(c6[, !"nm"], sub("T6_comparecimento_sinalizadas", "T6_comparecimento_por_tipo_local", arq), sep = ";", bom = TRUE)

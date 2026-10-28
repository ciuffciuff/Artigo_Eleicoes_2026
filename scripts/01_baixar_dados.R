# =============================================================================
# 01_baixar_dados.R - Download e extração dos dados do TSE (com verificação)
#
# Baixa:
#   * votacao_secao_2026_BR.zip    - votos para Presidente em todas as seções
#                                    (país + exterior), 1º e 2º turno
#   * detalhe_votacao_secao_2026.zip - eleitores aptos e comparecimento (T7)
# Os arquivos por UF (só cargos estaduais) NÃO são baixados: ocupam ~8 GB
# e não são usados. Para incluí-los, mude baixar_estaduais para TRUE.
#
# Pode rodar quantas vezes quiser: pula o que já está baixado e íntegro,
# e refaz o que estiver incompleto.
# =============================================================================
source("00_config.R")

baixar_estaduais <- FALSE

etapa("Espaço em disco")
exigir_espaco(if (baixar_estaduais) 20 else 6)

baixar <- function(url, destino) {
  if (file.exists(destino) && !is.null(tryCatch(zip::zip_list(destino), error = function(e) NULL))) {
    message("Já baixado: ", basename(destino)); return(TRUE)
  }
  if (file.exists(destino)) { message("ZIP corrompido, baixando de novo: ", basename(destino)); file.remove(destino) }
  message("Baixando ", basename(destino), " ...")
  temp <- paste0(destino, ".parcial")
  ok <- tryCatch({ download.file(url, temp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { message("  Falhou: ", conditionMessage(e)); FALSE })
  if (!ok) { if (file.exists(temp)) file.remove(temp); return(FALSE) }
  file.rename(temp, destino)
  if (is.null(tryCatch(zip::zip_list(destino), error = function(e) NULL))) {
    message("  ZIP baixado está corrompido: ", basename(destino)); file.remove(destino); return(FALSE)
  }
  TRUE
}

extrair <- function(arquivo_zip, pasta_destino) {
  lst <- zip::zip_list(arquivo_zip)
  precisa_gb <- sum(lst$uncompressed_size) / 1e9
  for (i in seq_len(nrow(lst))) {
    alvo <- file.path(pasta_destino, lst$filename[i])
    if (file.exists(alvo) && file.size(alvo) == lst$uncompressed_size[i]) next
    exigir_espaco(lst$uncompressed_size[i] / 1e9 + 0.5, pasta_destino)
    message("  Extraindo ", lst$filename[i], sprintf(" (%.2f GB)", lst$uncompressed_size[i] / 1e9))
    zip::unzip(arquivo_zip, files = lst$filename[i], exdir = pasta_destino)
  }
  ok <- csv_integro(arquivo_zip, pasta_destino)
  message(if (ok) "  OK: extração íntegra" else "  ERRO: extração incompleta")
  ok
}

falhas <- character(0)

etapa("Votação por seção - Presidente (BR)")
arquivos <- if (baixar_estaduais) c("BR", setdiff(ufs_esperadas, "ZZ")) else "BR"
for (a in arquivos) {
  z <- file.path(pasta_zip, sprintf("votacao_secao_%d_%s.zip", ano, a))
  if (!baixar(paste0(url_base_votacao, basename(z)), z) || !extrair(z, pasta_csv))
    falhas <- c(falhas, a)
  Sys.sleep(2)   # gentileza com o servidor
}

etapa("Detalhe da apuração por seção (aptos e comparecimento)")
zd <- file.path(pasta_zip, basename(url_detalhe))
if (!baixar(url_detalhe, zd) || !extrair(zd, pasta_det)) falhas <- c(falhas, "detalhe")

etapa("Resultado")
if (length(falhas)) {
  stop("Falharam: ", paste(falhas, collapse = ", "), ". Rode este script de novo.")
} else {
  message("Tudo baixado e íntegro. Próximo passo: 02_verificar_carga.R")
}

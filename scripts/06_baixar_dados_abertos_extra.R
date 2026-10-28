# =============================================================================
# 06_baixar_dados_abertos_extra.R - Dados abertos para os testes 6 a 12
#
# Baixa (portal de dados abertos do TSE), com verificação:
#   * perfil_eleitor_secao_2026_<UF>.zip  - perfil do eleitorado por seção
#     (sexo, faixa etária, escolaridade, biometria)              -> teste 7
#   * eleitorado_local_votacao_2026.zip   - locais de votação com
#     latitude/longitude                                          -> teste 10
#   * votacao_secao_2022_BR.zip           - Presidente 2022 por seção -> teste 8
# O modelo da urna (teste 6) já vem no detalhe baixado pelo 01.
#
# Se uma URL padrão falhar, o script procura o arquivo na API do portal
# (CKAN) e tenta de novo. Pode rodar quantas vezes quiser.
# Espaço: ~15 GB (ZIPs + CSVs).
# =============================================================================
source("00_config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

etapa("Espaço em disco")
exigir_espaco(15)

url_odsele <- "https://cdn.tse.jus.br/estatistica/sead/odsele/"
pastas <- list(perfil = file.path(pasta_extra, "perfil"),
               locais = file.path(pasta_extra, "locais"),
               v2022  = file.path(pasta_extra, "votacao2022"))
for (p in pastas) dir.create(p, recursive = TRUE, showWarnings = FALSE)
pasta_zip_extra <- file.path(pasta_extra, "zip")
dir.create(pasta_zip_extra, recursive = TRUE, showWarnings = FALSE)

# Procura no portal (API CKAN) a URL de um recurso cujo endereço contenha 'nome'
url_ckan <- function(dataset, nome) {
  api <- paste0("https://dadosabertos.tse.jus.br/api/3/action/package_show?id=", dataset)
  res <- tryCatch(jsonlite::fromJSON(api)$result$resources, error = function(e) NULL)
  if (is.null(res) || !nrow(res)) return(NA_character_)
  u <- res$url[grepl(nome, res$url, fixed = TRUE)]
  if (length(u)) u[1] else NA_character_
}

obter <- function(url, dataset, destino_pasta, padrao = NULL) {
  z <- file.path(pasta_zip_extra, basename(url))
  ok <- baixar_zip(url, z)
  if (!ok) {
    alt <- url_ckan(dataset, basename(url))
    if (!is.na(alt) && alt != url) { message("  Tentando endereço do portal: ", alt); ok <- baixar_zip(alt, z) }
  }
  ok && extrair_zip(z, destino_pasta, padrao)
}

falhas <- character(0)

etapa("Perfil do eleitorado por seção (28 arquivos)")
for (uf in ufs_esperadas) {
  u <- sprintf("%sperfil_eleitor_secao/perfil_eleitor_secao_%d_%s.zip", url_odsele, ano, uf)
  if (!obter(u, "eleitorado-2026", pastas$perfil, "\\.csv$")) falhas <- c(falhas, paste("perfil", uf))
  Sys.sleep(1)
}

etapa("Locais de votação (coordenadas)")
u <- sprintf("%seleitorado_locais_votacao/eleitorado_local_votacao_%d.zip", url_odsele, ano)
if (!obter(u, "eleitorado-2026", pastas$locais, "\\.csv$")) falhas <- c(falhas, "locais")

etapa("Votação por seção 2022 - Presidente (BR)")
u <- paste0(url_base_votacao, "votacao_secao_2022_BR.zip")
if (!obter(u, "resultados-2022", pastas$v2022, "\\.csv$")) falhas <- c(falhas, "votacao 2022")

# ---- Teste de carga dos arquivos novos ------------------------------------------
etapa("Teste de carga")
cabecalho <- function(f) names(fread(f, sep = ";", encoding = "Latin-1", nrows = 0))
checar <- function(rotulo, arqs, obrig) {
  if (!length(arqs)) { message("  ", rotulo, ": NENHUM CSV"); return(FALSE) }
  falta <- setdiff(obrig, cabecalho(arqs[1]))
  message(sprintf("  %-22s %d CSV(s)  %s", rotulo, length(arqs),
                  if (length(falta)) paste("FALTAM:", paste(falta, collapse = ", ")) else "colunas OK"))
  if (length(falta)) message("    colunas encontradas: ", paste(cabecalho(arqs[1]), collapse = ", "))
  !length(falta)
}
perf <- list.files(pastas$perfil, "\\.csv$", full.names = TRUE)
ufs_perf <- toupper(sub(".*_([A-Za-z]{2})\\.csv$", "\\1", perf))
sem <- setdiff(ufs_esperadas, ufs_perf)
if (length(sem)) message("  Perfil sem as UFs: ", paste(sem, collapse = ", "))
# a contagem chama QT_ELEITORES em 2026 (QT_ELEITORES_PERFIL em anos anteriores)
col_qt <- if (length(perf) && "QT_ELEITORES" %in% cabecalho(perf[1])) "QT_ELEITORES" else "QT_ELEITORES_PERFIL"
c1 <- checar("perfil do eleitorado", perf, c(sec, col_qt))
c2 <- checar("locais de votação", list.files(pastas$locais, "\\.csv$", full.names = TRUE),
             c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_LATITUDE", "NR_LONGITUDE"))
c3 <- checar("votação 2022", list.files(pastas$v2022, "_BR\\.csv$", full.names = TRUE),
             c(sec, "NR_TURNO", "NR_VOTAVEL", "QT_VOTOS"))

etapa("Resultado")
if (length(falhas) || !all(c1, c2, c3)) {
  stop("Problemas: ", paste(c(falhas, if (!c1) "colunas do perfil", if (!c2) "colunas dos locais",
                              if (!c3) "colunas de 2022"), collapse = "; "),
       ". Rode de novo; se persistir, me mande a mensagem acima.")
}
message("Tudo baixado e íntegro. Próximo passo: 07_testes_modelo_perfil_painel.R")

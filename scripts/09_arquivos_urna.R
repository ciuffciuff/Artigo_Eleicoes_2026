# =============================================================================
# 09_arquivos_urna.R - Roda, de dentro do R, a parte em Python dos testes
#                      com os arquivos de urna (N1, N2, N4 e N5)
#
# Por que Python: os arquivos da urna (BU, RDV, assinatura) são binários no
# formato ASN.1 e as assinaturas usam curvas Ed521/ECDSA P-521. Não há pacote
# R que leia isso; em Python há (asn1tools, cryptography).
#
# O que este script faz, sozinho:
#   1. instala o pacote R 'reticulate' (se faltar);
#   2. encontra um Python >= 3.11 no computador ou INSTALA um (só na 1ª vez);
#   3. cria um ambiente Python isolado "eleicoes2026" com asn1tools,
#      cryptography e py7zr (só na 1ª vez);
#   4. roda 09_arquivos_urna.py com esse Python, mostrando o progresso aqui
#      no console do R e gravando o log em resultados/log_09_python.txt.
#
# O .py roda como processo separado (e não dentro da sessão do R) porque usa
# vários núcleos do processador em paralelo, o que não funciona com o Python
# embutido no R no Windows.
#
# Saídas: tse_2026/urna/saida/*.csv (lidos pelo 10_testes_arquivos_urna.R)
# Tempo: o download dos ZIPs (~95 GB por turno) é a parte demorada; o
# processamento leva algumas horas com 8 núcleos.
# =============================================================================
source("00_config.R")

# ---- Parâmetros (ajuste aqui) ---------------------------------------------------
ufs          <- ufs_esperadas   # ex.: c("AC", "RR") para um teste rápido
turnos       <- c(1, 2)
modo         <- "zip"           # "zip" = todas as seções; "api" = amostra pelo portal Resultados
amostra      <- 3000            # só no modo "api"
processos    <- max(1, parallel::detectCores() - 1)
sem_assinatura <- FALSE         # TRUE confere só os hashes (bem mais rápido)
apagar_zip   <- FALSE           # TRUE apaga cada ZIP depois de processado
ambiente     <- "eleicoes2026"

if (!file.exists("09_arquivos_urna.py")) stop("09_arquivos_urna.py não está na pasta do projeto.")
if (!dir.exists("urna_vendor")) stop("Pasta urna_vendor/ não encontrada (vem no ZIP dos scripts).")
if (modo == "zip") exigir_espaco(if (apagar_zip) 15 else 100 * length(turnos), ".")

# ---- 1. reticulate --------------------------------------------------------------
etapa("Preparando o Python")
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
if (utils::packageVersion("reticulate") < "1.34")
  stop("Atualize o reticulate: install.packages('reticulate')")

# ---- 2 e 3. Python e ambiente isolado -------------------------------------------
pacotes_py <- c("asn1tools", "cryptography", "py7zr")
if (!reticulate::virtualenv_exists(ambiente)) {
  inicial <- reticulate::virtualenv_starter(">=3.11")
  if (is.null(inicial)) {
    message("Nenhum Python >= 3.11 encontrado. Instalando o Python 3.12 (só desta vez)...")
    inicial <- reticulate::install_python("3.12:latest")
  }
  message("Criando o ambiente '", ambiente, "' com ", paste(pacotes_py, collapse = ", "), " ...")
  reticulate::virtualenv_create(ambiente, python = inicial, packages = pacotes_py)
}
py <- reticulate::virtualenv_python(ambiente)
message("Python: ", py)

# teste de carga do ambiente: os pacotes importam?
teste <- suppressWarnings(system2(py, c("-c", shQuote("import asn1tools, cryptography, py7zr; print('ok')")),
                                  stdout = TRUE, stderr = TRUE))
if (!any(teste == "ok")) {
  message("Reinstalando pacotes Python ...")
  reticulate::virtualenv_install(ambiente, pacotes_py, ignore_installed = TRUE)
}

# ---- 4. Executar o .py ------------------------------------------------------------
etapa("Executando 09_arquivos_urna.py")
args <- c("-u", "09_arquivos_urna.py", "--modo", modo, "--processos", processos,
          "--ufs", ufs, "--turnos", turnos, "--pasta", file.path(pasta_base, "urna"))
if (modo == "api") args <- c(args, "--amostra", amostra)
if (sem_assinatura) args <- c(args, "--sem-assinatura")
if (apagar_zip) args <- c(args, "--apagar-zip")
arq_log <- file.path(pasta_saida, "log_09_python.txt")
message("Comando: ", py, " ", paste(args, collapse = " "))
message("Log: ", arq_log, "  (pode interromper com Esc e rodar de novo: retoma de onde parou)")

# processx mostra a saída do Python linha a linha no console do R
if (!requireNamespace("processx", quietly = TRUE)) install.packages("processx")
con_log <- file(arq_log, open = "at", encoding = "UTF-8")
res <- processx::run(py, as.character(args), echo = TRUE, error_on_status = FALSE,
                     stdout_line_callback = function(l, proc) writeLines(l, con_log),
                     stderr_line_callback = function(l, proc) writeLines(l, con_log),
                     env = c("current", PYTHONIOENCODING = "utf-8"))
close(con_log)
if (res$status != 0) stop("O Python terminou com erro (código ", res$status, "). Veja ", arq_log)

# ---- Teste de carga das saídas ------------------------------------------------------
etapa("Conferindo as saídas")
pasta_out <- file.path(pasta_base, "urna", "saida")
feitas <- list.files(pasta_out, "^secoes_t[12]_.*\\.csv$")
message("  Arquivos de seções gerados: ", length(feitas))
esperadas <- if (modo == "zip") as.vector(outer(paste0("secoes_t", turnos, "_"), paste0(ufs, ".csv"), paste0)) else character(0)
faltam <- setdiff(esperadas, feitas)
if (length(faltam)) message("  Ainda faltam: ", paste(faltam, collapse = ", "),
                            "\n  (ZIP não publicado ou erro de rede; rode de novo mais tarde ou use modo = \"api\")")
message("Concluído. Próximo passo: 10_testes_arquivos_urna.R")

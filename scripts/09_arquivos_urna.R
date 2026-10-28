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
#   2. encontra um Python >= 3.11 no computador ou instala o Miniconda
#      (só na 1ª vez; não precisa de git nem de administrador);
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
# Código de leitura dos arquivos de urna (DataUrnas-BR, licença MIT). Se a pasta
# urna_vendor/ não estiver aqui, baixa do GitHub público na versão fixada.
if (!file.exists(file.path("urna_vendor", "src", "dataurnas", "parsers", "bu.py"))) {
  etapa("Baixando o código DataUrnas-BR (só na 1ª vez)")
  commit <- "12f0433bd20882d305dd7b9e4987d6dcf3b321a9"
  url_v  <- sprintf("https://github.com/alissonlinneker/DataUrnas-BR/archive/%s.zip", commit)
  zv <- tempfile(fileext = ".zip"); dv <- tempfile()
  ok <- tryCatch({ download.file(url_v, zv, mode = "wb", quiet = TRUE); TRUE }, error = function(e) FALSE)
  if (!ok) stop("Não consegui baixar o DataUrnas-BR. Descompacte urna_vendor.zip (enviado na conversa) ",
                "na pasta do projeto, ao lado do 09_arquivos_urna.py, e rode de novo.")
  zip::unzip(zv, exdir = dv)
  raiz <- list.dirs(dv, recursive = FALSE)[1]
  dir.create("urna_vendor", showWarnings = FALSE)
  file.copy(file.path(raiz, c("src", "spec", "LICENSE")), "urna_vendor", recursive = TRUE)
  unlink(file.path("urna_vendor", "src", "dataurnas", c("dashboard", "database")), recursive = TRUE)
  if (!file.exists(file.path("urna_vendor", "src", "dataurnas", "parsers", "bu.py")))
    stop("Download do DataUrnas-BR incompleto. Use o urna_vendor.zip enviado na conversa.")
  message("  urna_vendor/ criada (DataUrnas-BR, commit ", substr(commit, 1, 7), ")")
}
if (modo == "zip") exigir_espaco(if (apagar_zip) 15 else 100 * length(turnos), ".")

# ---- 1. reticulate --------------------------------------------------------------
etapa("Preparando o Python")
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
if (utils::packageVersion("reticulate") < "1.34")
  stop("Atualize o reticulate: install.packages('reticulate')")

# ---- 2 e 3. Python e ambiente isolado -------------------------------------------
pacotes_py <- c("asn1tools", "cryptography", "py7zr")
# Ordem de tentativa:
#   a) ambiente já criado em execução anterior (virtualenv ou conda);
#   b) Python >= 3.11 já instalado no computador -> cria um virtualenv;
#   c) nenhum Python: instala o Miniconda (não precisa de git nem de
#      permissão de administrador) e cria um ambiente conda com Python 3.12.
#      O install_python() do reticulate exige git no Windows, por isso não é usado.
py <- NULL
if (suppressWarnings(reticulate::virtualenv_exists(ambiente))) {
  py <- suppressWarnings(reticulate::virtualenv_python(ambiente))
} else if (!is.null(tryCatch(reticulate::conda_binary(), error = function(e) NULL)) &&
           reticulate::condaenv_exists(ambiente)) {
  py <- reticulate::conda_python(ambiente)
} else {
  # No Windows, "WindowsApps\python3.exe" é só um atalho da Microsoft Store (não é um
  # Python de verdade) e gera o aviso "não é possível abrir o arquivo". É ignorado.
  inicial <- suppressWarnings(tryCatch(reticulate::virtualenv_starter(">=3.11"), error = function(e) NULL))
  if (!is.null(inicial) && grepl("WindowsApps", inicial, fixed = TRUE)) inicial <- NULL
  if (!is.null(inicial)) {
    message("Criando o ambiente '", ambiente, "' com ", paste(pacotes_py, collapse = ", "), " ...")
    reticulate::virtualenv_create(ambiente, python = inicial, packages = pacotes_py)
    py <- reticulate::virtualenv_python(ambiente)
  } else {
    if (is.null(tryCatch(reticulate::conda_binary(), error = function(e) NULL))) {
      message("Nenhum Python >= 3.11 encontrado. Instalando o Miniconda (só desta vez, ~100 MB) ...")
      reticulate::install_miniconda()
    }
    message("Criando o ambiente conda '", ambiente, "' com Python 3.12 e ", paste(pacotes_py, collapse = ", "), " ...")
    reticulate::conda_create(ambiente, packages = "python=3.12")
    reticulate::conda_install(ambiente, pacotes_py, pip = TRUE)
    py <- reticulate::conda_python(ambiente)
  }
}
message("Python: ", py)

# teste de carga do ambiente: os pacotes importam?
teste <- suppressWarnings(system2(py, c("-c", shQuote("import asn1tools, cryptography, py7zr; print('ok')")),
                                  stdout = TRUE, stderr = TRUE))
if (!any(teste == "ok")) {
  message("Reinstalando pacotes Python ...")
  system2(py, c("-m", "pip", "install", "--upgrade", pacotes_py))
  teste <- suppressWarnings(system2(py, c("-c", shQuote("import asn1tools, cryptography, py7zr; print('ok')")),
                                    stdout = TRUE, stderr = TRUE))
  if (!any(teste == "ok")) stop("Pacotes Python não instalaram: ", paste(teste, collapse = " "))
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

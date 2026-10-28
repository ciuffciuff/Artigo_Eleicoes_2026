# =============================================================================
# 00_config.R - Configuração e funções comuns a todos os scripts
# Não precisa rodar sozinho: os scripts 01 a 10 carregam este arquivo.
#
# ANTES DE COMEÇAR
#   * Coloque a pasta do projeto FORA do OneDrive, de preferência no disco D:
#     (ex.: D:/dados/Estudo_eleicao). O OneDrive duplica e sincroniza arquivos
#     de vários GB e enche o disco.
#   * Abra o RStudio nessa pasta (Session > Set Working Directory > To Source
#     File Location). Todos os caminhos abaixo são relativos a ela.
#   * Espaço livre necessário:
#       - scripts 01 a 05 ........................ ~6 GB
#       - scripts 06 a 08 (dados abertos extras) .. ~15 GB a mais
#       - script 09 (arquivos de urna, 2 turnos) .. ~200 GB a mais
# =============================================================================

# ---- Pacotes ------------------------------------------------------------------
for (p in c("data.table", "zip")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages(library(data.table))

# ---- Parâmetros ---------------------------------------------------------------
ano <- 2026

pasta_base    <- "tse_2026"
pasta_zip     <- file.path(pasta_base, "zip")
pasta_csv     <- file.path(pasta_base, "csv")
pasta_det     <- file.path(pasta_base, "detalhe")
pasta_detalhe <- pasta_det                      # nome antigo, mantido
pasta_saida   <- "resultados"
for (d in c(pasta_zip, pasta_csv, pasta_det, pasta_saida))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

url_base_votacao <- "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/"
url_detalhe      <- sprintf("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_%d.zip", ano)
arquivo_br       <- file.path(pasta_csv, sprintf("votacao_secao_%d_BR.csv", ano))

ufs_esperadas <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS",
                   "MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC",
                   "SE","SP","TO","ZZ")               # ZZ = exterior
especiais <- c(95L, 96L, 97L)                        # branco, nulo, anulado
sec       <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")   # uma seção
chave     <- c("NR_TURNO", sec)                                  # seção em um turno

options(timeout = 7200)   # downloads grandes; o padrão do R é 60 s

# ---- Funções auxiliares -------------------------------------------------------
etapa  <- function(...) message("\n==== ", ..., " ====")
salvar <- function(dt, nome) fwrite(dt, file.path(pasta_saida, nome), sep = ";", bom = TRUE)
z_rob  <- function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)

# Espaço livre (GB) no disco onde está a pasta; NA se não conseguir medir
espaco_livre_gb <- function(caminho = ".") {
  caminho <- normalizePath(caminho, winslash = "/", mustWork = FALSE)
  tryCatch({
    if (.Platform$OS.type == "windows") {
      letra <- substr(caminho, 1, 1)
      out <- system2("powershell", c("-NoProfile", "-Command",
                     sprintf("(Get-PSDrive -Name %s).Free", letra)), stdout = TRUE)
      as.numeric(out[length(out)]) / 1e9
    } else {
      out <- system2("df", c("-k", shQuote(caminho)), stdout = TRUE)
      as.numeric(strsplit(trimws(out[length(out)]), "\\s+")[[1]][4]) * 1024 / 1e9
    }
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

exigir_espaco <- function(gb, caminho = pasta_base) {
  livre <- espaco_livre_gb(caminho)
  if (is.na(livre)) { message("Não consegui medir o espaço livre; seguindo."); return(invisible(TRUE)) }
  message(sprintf("Espaço livre no disco: %.1f GB (necessário: %.1f GB)", livre, gb))
  if (livre < gb) stop(sprintf(paste0("Espaço insuficiente: %.1f GB livres, %.1f GB necessários. ",
                       "Libere espaço ou mova a pasta para outro disco."), livre, gb))
  invisible(TRUE)
}

# CSV extraído inteiro? (compara com o tamanho registrado dentro do ZIP)
csv_integro <- function(arquivo_zip, pasta_destino) {
  lst <- tryCatch(zip::zip_list(arquivo_zip), error = function(e) NULL)
  if (is.null(lst)) return(FALSE)
  f <- file.path(pasta_destino, lst$filename)
  all(file.exists(f)) && all(file.size(f) == lst$uncompressed_size)
}

# Lê um CSV do TSE: só as colunas pedidas que existirem, textos em UTF-8
ler_csv_tse <- function(arquivo, colunas = NULL) {
  if (!file.exists(arquivo)) stop("Arquivo não encontrado: ", arquivo)
  ex  <- names(fread(arquivo, sep = ";", encoding = "Latin-1", nrows = 0))
  sel <- if (is.null(colunas)) NULL else intersect(colunas, ex)
  dt  <- fread(arquivo, sep = ";", encoding = "Latin-1", select = sel, showProgress = FALSE)
  for (cc in names(dt)[vapply(dt, is.character, logical(1))])
    set(dt, j = cc, value = enc2utf8(dt[[cc]]))
  dt
}

# ---- Teste de carga: votos de Presidente ---------------------------------------
# Para o script com erro claro se a carga estiver incompleta.
testar_carga_presidente <- function(pres, exigir_dois_turnos = TRUE) {
  erros <- character(0); avisos <- character(0)
  if (nrow(pres) == 0) stop("TESTE DE CARGA: nenhuma linha de Presidente.")
  obrig <- c(sec, "NR_TURNO", "NR_VOTAVEL", "QT_VOTOS")
  falta <- setdiff(obrig, names(pres))
  if (length(falta)) stop("TESTE DE CARGA: faltam colunas: ", paste(falta, collapse = ", "))
  turnos <- sort(unique(pres$NR_TURNO))
  if (exigir_dois_turnos && !all(c(1L, 2L) %in% turnos))
    erros <- c(erros, paste("turnos presentes:", paste(turnos, collapse = ", ")))
  for (tr in turnos) {
    sem <- setdiff(ufs_esperadas, unique(pres[NR_TURNO == tr, SG_UF]))
    if (length(sem)) erros <- c(erros, sprintf("%dº turno sem as UFs: %s", tr, paste(sem, collapse = ", ")))
  }
  extra <- setdiff(unique(pres$SG_UF), ufs_esperadas)
  if (length(extra)) avisos <- c(avisos, paste("UF inesperada:", paste(extra, collapse = ", ")))
  if (anyNA(pres$QT_VOTOS) || any(pres$QT_VOTOS < 0, na.rm = TRUE))
    erros <- c(erros, "QT_VOTOS com valor ausente ou negativo")
  resumo <- pres[, .(UFs = uniqueN(SG_UF), secoes = uniqueN(.SD),
                     votos = sum(as.numeric(QT_VOTOS))), by = NR_TURNO, .SDcols = sec]
  message("Teste de carga (Presidente):")
  print(resumo)
  for (a in avisos) message("  aviso: ", a)
  if (length(erros)) stop("TESTE DE CARGA FALHOU:\n  - ", paste(erros, collapse = "\n  - "),
                          "\nRode 01_baixar_dados.R e 02_verificar_carga.R.")
  message("  OK: ", length(ufs_esperadas), " UFs (27 + exterior) em todos os turnos.")
  invisible(resumo)
}

# ---- Leitura padrão: votos de Presidente (arquivo BR) ---------------------------
ler_presidente <- function(colunas = c(sec, "NM_MUNICIPIO", "NR_LOCAL_VOTACAO",
                                       "NM_LOCAL_VOTACAO", "NR_TURNO", "DS_CARGO",
                                       "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS"),
                           exigir_dois_turnos = TRUE) {
  if (!file.exists(arquivo_br)) stop("Não encontrei ", arquivo_br, ". Rode 01_baixar_dados.R.")
  z <- file.path(pasta_zip, sprintf("votacao_secao_%d_BR.zip", ano))
  if (file.exists(z) && !csv_integro(z, pasta_csv))
    stop("TESTE DE CARGA: ", basename(arquivo_br), " está incompleto (cortado na extração). ",
         "Rode 01_baixar_dados.R de novo.")
  message("Lendo ", basename(arquivo_br), " ...")
  dt <- ler_csv_tse(arquivo_br, unique(c(colunas, "DS_CARGO")))
  cargos <- unique(dt$DS_CARGO)
  dt <- dt[toupper(DS_CARGO) == "PRESIDENTE"]
  if (nrow(dt) == 0) stop("Nenhuma linha de Presidente. Cargos no arquivo: ", paste(cargos, collapse = ", "))
  for (cc in intersect(c("NR_TURNO", "NR_VOTAVEL", "QT_VOTOS"), names(dt)))
    set(dt, j = cc, value = as.integer(dt[[cc]]))
  for (cc in c("NM_LOCAL_VOTACAO", "NM_MUNICIPIO", "NR_LOCAL_VOTACAO"))
    if (!cc %in% names(dt)) dt[, (cc) := NA]
  testar_carga_presidente(dt, exigir_dois_turnos)
  dt
}

# ---- Leitura padrão: detalhe da apuração (aptos, comparecimento) ---------------
# Retorna NULL (com aviso) se o detalhe não estiver disponível.
# extras: outras colunas do detalhe a manter, se existirem (ex.: "DS_MODELO_URNA")
ler_detalhe <- function(extras = character(0)) {
  arqs <- list.files(pasta_det, "^detalhe_votacao_secao.*\\.csv$", full.names = TRUE)
  if (!length(arqs)) { message("Detalhe não encontrado em ", pasta_det); return(NULL) }
  z <- file.path(pasta_zip, basename(url_detalhe))
  if (file.exists(z) && !csv_integro(z, pasta_det))
    stop("TESTE DE CARGA: CSVs do detalhe incompletos. Rode 01_baixar_dados.R de novo.")
  # o ZIP pode ter um arquivo BR (Presidente) e os por UF: se houver BR, usa só ele
  if (any(grepl("_BR\\.csv$", arqs))) arqs <- arqs[grepl("_BR\\.csv$", arqs)]
  message("Lendo detalhe: ", paste(basename(arqs), collapse = ", "))
  det <- rbindlist(lapply(arqs, ler_csv_tse,
                          colunas = c(chave, "DS_CARGO", "QT_APTOS", "QT_COMPARECIMENTO", extras)),
                   fill = TRUE)
  falta <- setdiff(c(chave, "QT_APTOS", "QT_COMPARECIMENTO"), names(det))
  if (length(falta)) { message("Detalhe sem as colunas: ", paste(falta, collapse = ", ")); return(NULL) }
  if ("DS_CARGO" %in% names(det)) det <- det[toupper(DS_CARGO) == "PRESIDENTE"]
  extras <- intersect(extras, names(det))
  det <- det[, c(chave, "QT_APTOS", "QT_COMPARECIMENTO", extras), with = FALSE]
  for (cc in c("NR_TURNO", "QT_APTOS", "QT_COMPARECIMENTO")) set(det, j = cc, value = as.integer(det[[cc]]))
  det <- unique(det)
  dup <- det[, .N, by = chave][N > 1]
  if (nrow(dup)) {
    message("Aviso: ", nrow(dup), " seções com linhas diferentes no detalhe; mantida a primeira.")
    det <- det[, .SD[1], by = chave]
  }
  sem <- setdiff(ufs_esperadas, unique(det$SG_UF))
  if (length(sem)) message("Aviso: detalhe sem as UFs: ", paste(sem, collapse = ", "))
  message("  Detalhe: ", nrow(det), " linhas (seção x turno)")
  det
}

# =============================================================================
# Funções usadas a partir do script 06 (novos testes)
# =============================================================================
pasta_extra <- file.path(pasta_base, "extra")     # perfil, locais, votação 2022
pasta_urna  <- file.path(pasta_base, "urna")      # arquivos de urna (script 09)

# Download com verificação do ZIP (pula se já baixado e íntegro)
baixar_zip <- function(url, destino) {
  ok_zip <- function(f) !is.null(tryCatch(zip::zip_list(f), error = function(e) NULL))
  if (file.exists(destino) && ok_zip(destino)) { message("Já baixado: ", basename(destino)); return(TRUE) }
  if (file.exists(destino)) file.remove(destino)
  message("Baixando ", basename(destino), " ...")
  temp <- paste0(destino, ".parcial")
  ok <- tryCatch({ download.file(url, temp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { message("  Falhou: ", conditionMessage(e)); FALSE },
                 warning = function(w) { message("  Falhou: ", conditionMessage(w)); FALSE })
  if (!ok) { if (file.exists(temp)) file.remove(temp); return(FALSE) }
  file.rename(temp, destino)
  if (!ok_zip(destino)) { message("  ZIP corrompido: ", basename(destino)); file.remove(destino); return(FALSE) }
  TRUE
}

# Extrai só o que falta (ou está incompleto); 'padrao' filtra os arquivos do ZIP
extrair_zip <- function(arquivo_zip, pasta_destino, padrao = NULL) {
  dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)
  lst <- zip::zip_list(arquivo_zip)
  if (!is.null(padrao)) lst <- lst[grepl(padrao, lst$filename), ]
  for (i in seq_len(nrow(lst))) {
    alvo <- file.path(pasta_destino, lst$filename[i])
    if (file.exists(alvo) && file.size(alvo) == lst$uncompressed_size[i]) next
    exigir_espaco(lst$uncompressed_size[i] / 1e9 + 0.5, pasta_destino)
    message("  Extraindo ", lst$filename[i], sprintf(" (%.2f GB)", lst$uncompressed_size[i] / 1e9))
    zip::unzip(arquivo_zip, files = lst$filename[i], exdir = pasta_destino)
  }
  f <- file.path(pasta_destino, lst$filename)
  ok <- all(file.exists(f)) && all(file.size(f) == lst$uncompressed_size)
  if (!ok) message("  ERRO: extração incompleta de ", basename(arquivo_zip))
  ok
}

# Correção de Benjamini-Hochberg (q-valor)
bh <- function(p) p.adjust(p, method = "BH")

# Logit empírico de y em n, com correção de 1/2 (evita ±Inf em 0% e 100%)
logit_emp <- function(y, n) log((y + 0.5) / (n - y + 0.5))

# Regressão linear com efeito fixo (transformação "within") e erro-padrão
# robusto por cluster. Base R, sem pacotes extras.
#   y, X: resposta e matriz/data.frame de covariáveis (sem intercepto)
#   fe:   vetor que identifica o grupo do efeito fixo (ex.: local de votação)
#   cl:   vetor do cluster para o erro-padrão (ex.: município)
#   w:    pesos (opcional)
fe_lm <- function(y, X, fe, cl = fe, w = NULL) {
  X <- as.matrix(X)
  if (is.null(w)) w <- rep(1, length(y))
  ok <- stats::complete.cases(y, X, fe, cl, w) & w > 0
  y <- y[ok]; X <- X[ok, , drop = FALSE]; fe <- fe[ok]; cl <- cl[ok]; w <- w[ok]
  dm <- function(v) v - ave(v * w, fe, FUN = sum) / ave(w, fe, FUN = sum)
  yd <- dm(y); Xd <- apply(X, 2, dm)
  if (is.null(dim(Xd))) Xd <- matrix(Xd, ncol = ncol(X))
  colnames(Xd) <- colnames(X)
  keep <- colSums(Xd^2) > 1e-12                # retira colunas sem variação no grupo
  Xd <- Xd[, keep, drop = FALSE]
  sw <- sqrt(w)
  fit <- lm.fit(Xd * sw, yd * sw)
  b <- fit$coefficients
  u <- (yd - Xd %*% b)[, 1]
  XtXi <- solve(crossprod(Xd * sw))
  sc <- rowsum(Xd * (w * u), cl)                # escores por cluster
  G <- nrow(sc); n <- length(y); k <- ncol(Xd) + length(unique(fe))
  corr <- (G / (G - 1)) * ((n - 1) / max(n - k, 1))
  V <- corr * XtXi %*% crossprod(sc) %*% XtXi
  se <- sqrt(diag(V))
  data.table(termo = names(b), coef = b, ep = se, z = b / se,
             p = 2 * pnorm(-abs(b / se)), n = n, grupos_fe = length(unique(fe)),
             clusters = G)
}

# Proporção de votos de cada candidato por seção (2 finalistas) e totais,
# no formato largo. Usa o arquivo BR já lido por ler_presidente().
largo_finalistas <- function(pres, cand_a = 13L, cand_b = 22L) {
  pres[, .(lula   = sum(QT_VOTOS[NR_VOTAVEL == cand_a]),
           flavio = sum(QT_VOTOS[NR_VOTAVEL == cand_b]),
           validos = sum(QT_VOTOS[!NR_VOTAVEL %in% especiais]),
           total  = sum(QT_VOTOS),
           NM_MUNICIPIO = NM_MUNICIPIO[1],
           NR_LOCAL_VOTACAO = NR_LOCAL_VOTACAO[1],
           NM_LOCAL_VOTACAO = NM_LOCAL_VOTACAO[1]),
       by = chave]
}

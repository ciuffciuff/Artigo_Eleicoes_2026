# =============================================================================
# 99_executar_tudo.R - Roda toda a sequência, na ordem, parando no 1º erro
#   01 baixar -> 02 verificar carga -> 03 distribuição -> 04 testes T1-T7
#   -> 05 cruzamento T8
#   -> 06 baixar dados abertos extras -> 07 novos testes 6, 7, 8
#   -> 08 novos testes 9, 10, 11, 12
#   -> 09 arquivos de urna (Python, chamado pelo R) -> 10 novos testes 1, 2, 4, 5
#   -> 11 novo teste 13 (liberação biográfica; usa o que o 09 já extraiu)
# Cada etapa grava seu log em resultados/log_<script>.txt
# =============================================================================
pular_download      <- FALSE  # TRUE se 01 já rodou e os dados estão íntegros
rodar_originais     <- TRUE   # 02 a 05 (FALSE se já rodaram e nada mudou)
rodar_novos_abertos <- TRUE   # 06 a 08
rodar_arquivos_urna <- TRUE   # 09 e 10 (download de ~95 GB por turno; horas de execução)
rodar_n13           <- TRUE   # 11 (minutos; só precisa da saída do 09, sem novo download)

scripts <- c(if (!pular_download) "01_baixar_dados.R",
             if (rodar_originais) c("02_verificar_carga.R", "03_distribuicao_presidente.R",
                                    "04_testes_forenses.R", "05_cruzamento_comparecimento_perfil.R"),
             if (rodar_novos_abertos) c("06_baixar_dados_abertos_extra.R", "07_testes_modelo_perfil_painel.R",
                                        "08_testes_padroes_espaciais_ml.R"),
             if (rodar_arquivos_urna) c("09_arquivos_urna.R", "10_testes_arquivos_urna.R"),
             if (rodar_n13) "11_teste_liberacao_biografica.R")

dir.create("resultados", showWarnings = FALSE)
for (s in scripts) {
  if (!file.exists(s)) stop("Script não encontrado: ", s, " (rode na pasta do projeto)")
  message("\n############ ", s, " ############")
  inicio <- Sys.time()
  log <- file(file.path("resultados", paste0("log_", sub("\\.R$", "", s), ".txt")), open = "wt")
  sink(log, type = "output", split = TRUE)
  ok <- tryCatch({ source(s, local = new.env(), echo = FALSE); TRUE },
                 error = function(e) { message("ERRO em ", s, ": ", conditionMessage(e)); FALSE })
  sink(type = "output"); close(log)
  message(sprintf("%s: %s em %.1f min", s, if (ok) "OK" else "FALHOU",
                  as.numeric(difftime(Sys.time(), inicio, units = "mins"))))
  if (!ok) stop("Sequência interrompida em ", s, ". Corrija e rode de novo (as etapas já feitas são puladas ou refeitas rapidamente).")
}
message("\nSequência completa. Resultados em: ", normalizePath("resultados"))

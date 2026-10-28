# =============================================================================
# 02_verificar_carga.R - Testes de carga antes das análises
#
# Verifica:
#   C1. integridade da extração (CSV com o tamanho registrado no ZIP);
#   C2. presença das 27 UFs + exterior (ZZ) nos dois turnos;
#   C3. nº de municípios, zonas, seções e votos por UF e turno;
#   C4. seções presentes em um turno e ausentes no outro;
#   C5. seções do detalhe (aptos) ausentes da votação, e o contrário;
#   C6. total de votos para Presidente = comparecimento da seção
#       (cada eleitor dá exatamente um voto para Presidente, contando
#        brancos e nulos).
# Diferenças em seções com comparecimento ZERO são esperadas: o arquivo de
# votação só traz seções que receberam votos.
#
# Saídas em resultados/: 02_carga_por_uf.csv, 02_secoes_divergentes.csv,
#                        02_votos_x_comparecimento.csv
# =============================================================================
source("00_config.R")

criticos <- character(0); avisos <- character(0)

# ---- C1. Integridade ----------------------------------------------------------
etapa("C1. Integridade da extração")
for (z in list.files(pasta_zip, "\\.zip$", full.names = TRUE)) {
  destino <- if (grepl("detalhe", basename(z))) pasta_det else pasta_csv
  ok <- csv_integro(z, destino)
  message(sprintf("  %-40s %s", basename(z), if (ok) "OK" else "INCOMPLETO"))
  if (!ok) {
    msg <- paste("CSV incompleto ou ausente:", basename(z))
    if (grepl("_BR\\.zip$|detalhe", basename(z))) criticos <- c(criticos, msg)
    else avisos <- c(avisos, paste(msg, "(cargos estaduais, não usado)"))
  }
}

# ---- C2/C3. Presença e volume por UF ------------------------------------------
etapa("C2/C3. UFs, turnos e volumes")
pres <- ler_csv_tse(arquivo_br, c(sec, "NR_TURNO", "DS_CARGO", "NR_VOTAVEL", "QT_VOTOS"))
pres <- pres[toupper(DS_CARGO) == "PRESIDENTE"]
pres[, `:=`(NR_TURNO = as.integer(NR_TURNO), QT_VOTOS = as.integer(QT_VOTOS))]
r <- tryCatch({ testar_carga_presidente(pres); NULL }, error = function(e) conditionMessage(e))
if (!is.null(r)) criticos <- c(criticos, r)

turnos <- sort(unique(pres$NR_TURNO))
carga <- pres[, .(municipios = uniqueN(CD_MUNICIPIO),
                  zonas      = uniqueN(paste(CD_MUNICIPIO, NR_ZONA)),
                  secoes     = uniqueN(paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)),
                  votos      = sum(as.numeric(QT_VOTOS))),
              by = .(UF = SG_UF, turno = NR_TURNO)]
carga <- merge(CJ(UF = ufs_esperadas, turno = turnos), carga, by = c("UF", "turno"), all = TRUE)
for (cc in c("municipios", "zonas", "secoes", "votos")) set(carga, which(is.na(carga[[cc]])), cc, 0)

# Uma linha por seção e turno, com o total de votos
vs <- pres[, .(votos = sum(QT_VOTOS)), by = chave]

# ---- C4. Seções em um turno e não no outro ------------------------------------
etapa("C4. Seções entre turnos")
div <- list()
if (all(c(1L, 2L) %in% turnos)) {
  s1 <- unique(vs[NR_TURNO == 1, ..sec]); s2 <- unique(vs[NR_TURNO == 2, ..sec])
  so1 <- s1[!s2, on = sec][, `:=`(tipo = "só no 1º turno", NR_TURNO = 2L)]
  so2 <- s2[!s1, on = sec][, `:=`(tipo = "só no 2º turno", NR_TURNO = 1L)]
  div$turnos <- rbind(so1, so2)     # NR_TURNO = turno em que a seção FALTA
  message("  Seções só no 1º turno: ", nrow(so1), " | só no 2º turno: ", nrow(so2))
}

# ---- C5/C6. Comparação com o detalhe ------------------------------------------
det <- ler_detalhe()
vc <- NULL
if (!is.null(det)) {
  etapa("C5. Votação x detalhe")
  so_det <- det[!vs, on = chave][, tipo := "no detalhe, sem votos na votação"]
  so_vot <- vs[!det, on = chave][, tipo := "na votação, ausente do detalhe"]
  message("  No detalhe e sem votos: ", nrow(so_det), " | na votação e fora do detalhe: ", nrow(so_vot))
  div$detalhe <- rbind(so_det[, c(chave, "tipo"), with = FALSE],
                       so_vot[, c(chave, "tipo"), with = FALSE])
  if (nrow(so_vot)) criticos <- c(criticos, paste(nrow(so_vot), "seções com votos e sem registro no detalhe"))

  cd <- det[, .(secoes_detalhe = .N, aptos = sum(as.numeric(QT_APTOS)),
                comparecimento = sum(as.numeric(QT_COMPARECIMENTO))),
            by = .(UF = SG_UF, turno = NR_TURNO)]
  carga <- merge(carga, cd, by = c("UF", "turno"), all.x = TRUE)

  etapa("C6. Total de votos x comparecimento, por seção")
  vc <- merge(vs, det, by = chave)
  vc[, diferenca := votos - QT_COMPARECIMENTO]
  res6 <- vc[, .(secoes = .N, iguais = sum(diferenca == 0),
                 diferentes = sum(diferenca != 0),
                 maior_diferenca = max(abs(diferenca))), by = .(UF = SG_UF, turno = NR_TURNO)]
  print(res6[diferentes > 0])
  message(sprintf("  Seções com votos = comparecimento: %d de %d (%.3f%%)",
                  sum(res6$iguais), sum(res6$secoes), 100 * sum(res6$iguais) / sum(res6$secoes)))
  if (sum(res6$diferentes) > 0)
    avisos <- c(avisos, paste(sum(res6$diferentes),
                              "seções com total de votos diferente do comparecimento (ver 02_votos_x_comparecimento.csv)"))
  salvar(vc[diferenca != 0], "02_votos_x_comparecimento.csv")
}

# ---- Classificação das seções divergentes -------------------------------------
divergentes <- rbindlist(div, fill = TRUE)
if (nrow(divergentes) && !is.null(det)) {
  divergentes <- merge(divergentes, det, by = chave, all.x = TRUE)
  divergentes[, explicacao := fifelse(!is.na(QT_COMPARECIMENTO) & QT_COMPARECIMENTO == 0,
                                      "explicada: comparecimento zero",
                                      "NÃO explicada: conferir no portal Resultados")]
  print(divergentes[, .N, by = .(SG_UF, tipo, explicacao)])
  n_nao <- divergentes[explicacao != "explicada: comparecimento zero", .N]
  if (n_nao) avisos <- c(avisos, paste(n_nao, "seções divergentes sem explicação (ver 02_secoes_divergentes.csv)"))
} else if (nrow(divergentes)) {
  avisos <- c(avisos, paste(nrow(divergentes), "seções presentes em só um turno (sem detalhe para explicar)"))
}
salvar(divergentes, "02_secoes_divergentes.csv")

# ---- Resultado ------------------------------------------------------------------
setorder(carga, turno, UF)
print(carga)
salvar(carga, "02_carga_por_uf.csv")

etapa("Resultado do teste de carga")
for (a in avisos)   message("  AVISO:   ", a)
for (c in criticos) message("  CRÍTICO: ", c)
if (length(criticos)) {
  stop("Carga com problemas críticos. Corrija (geralmente: rodar 01_baixar_dados.R de novo) antes de seguir.")
} else {
  message("Carga aprovada", if (length(avisos)) " com avisos" else "", ". Próximo passo: 03_distribuicao_presidente.R")
}

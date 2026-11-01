# =============================================================================
# 14_painel_dados.R - Dados do painel HTML (achados por UF e por seção)
#
# Junta, para cada seção eleitoral:
#   * resultado de Presidente nos dois turnos (TSE);
#   * perfil do eleitorado (TSE): sexo, idade, escolaridade, cor ou raça;
#   * perfil do Censo 2022 em volta do local de votação (IBGE, raio do 13);
#   * em quais testes a seção foi sinalizada (T1-T8, N4-N15).
# e grava arquivos JSON compactos em painel/dados/:
#   resumo.json   - Brasil e cada UF, descrição dos testes, tabelas do N15
#   uf_XX.json    - todas as seções da UF (colunas em vetores, para ficar leve)
#
# Pré-requisito: 13_enriquecimento_ibge.R rodado (usa as tabelas que ele
# grava em tse_2026/extra e resultados/N15_*.csv) e o CSV de votação do 01.
# =============================================================================
source("00_config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
cand_a <- 13L; cand_b <- 22L
pasta_painel <- file.path("..", "painel", "dados")
dir.create(pasta_painel, recursive = TRUE, showWarnings = FALSE)
achar <- function(f) {
  for (d in c(pasta_saida, file.path("..", "resultados"))) if (file.exists(file.path(d, f))) return(file.path(d, f))
  NA_character_
}
ri <- function(x, k = 1000) as.integer(round(x * k))     # proporção -> inteiro (por mil)
razao <- function(a, b) ifelse(b > 0, a / b, NA_real_)

# ---- Testes (ordem = bit no campo 'flags' de cada seção) -------------------------
testes <- data.table(
  cod = c("T1", "T5", "T6", "T6c", "T8", "N4", "N5", "N7", "N8", "N11", "N13", "N13p", "N14C", "N14D", "N15"),
  arquivo = c("T1_secoes_0_ou_100.csv", "T5_secoes_sinalizadas.csv", "T6_secoes_sinalizadas.csv",
              "T6_comparecimento_sinalizadas.csv", "T8_secoes_sinalizadas.csv", "N4_secoes_sinalizadas.csv",
              "N5_secoes_sinalizadas.csv", "N7_secoes_sinalizadas.csv", "N8_secoes_sinalizadas.csv",
              "N11_secoes_sinalizadas.csv", "N13_secoes_extremas_3_marcas.csv", "N13_secoes_extremas_painel.csv",
              "N14_secoes_sinalizadas_C.csv", "N14_secoes_acima_teto_D.csv", "N15_modelo_secoes_sinalizadas.csv"),
  nome = c("Zeros e 100%", "Homogeneidade no local", "Consistência 1º x 2º turno", "Comparecimento entre turnos",
           "Comparecimento x mudança de voto", "Ritmo de votação (log da urna)", "Liberação sem biometria",
           "Perfil do eleitorado (só TSE)", "Painel 2022 → 2026", "Floresta aleatória",
           "Liberação biográfica: 3 marcas", "Liberação biográfica: painel", "Ganho de Lula x reserva de esquerda",
           "Acima do teto de participação", "Perfil TSE + Censo 2022"),
  descricao = c(
    "Um finalista com 0 votos ou 100% dos válidos na seção.",
    "Resultado da seção muito diferente das outras seções do mesmo local de votação.",
    "Resultado do 2º turno foge do previsto pelo 1º turno da própria seção, em comparação com as vizinhas.",
    "Variação do comparecimento entre os turnos fora do padrão do município.",
    "Comparecimento E mudança de voto extremos ao mesmo tempo (marca de inflar seção).",
    "Votos fora do horário, rajadas ou intervalos curtos demais no log da urna.",
    "Proporção alta de eleitores habilitados sem biometria (por dado biográfico).",
    "Voto incoerente com sexo, idade, escolaridade e biometria dos eleitores (efeito fixo de município).",
    "Mudança do voto entre 2022 e 2026 na mesma seção muito fora do padrão do município.",
    "Modelo de aprendizado de máquina treinado com fraudes simuladas deu probabilidade alta.",
    "Mais liberações biográficas, mais comparecimento e mais votos no fim do dia, juntos.",
    "As mesmas três marcas, comparando 1º e 2º turno na mesma seção.",
    "Ganho de Lula entre turnos fora do previsto pelo voto de esquerda disponível no 1º turno.",
    "Votos de um finalista acima do teto possível dado o 1º turno da seção (explicativo, não detecta fraude).",
    "Ainda incoerente depois de incluir renda, cor ou raça, escolaridade e território do Censo 2022 (raio de 1 km)."))
testes[, bit := seq_len(.N) - 1L]

flags <- list()
for (i in seq_len(nrow(testes))) {
  f <- achar(testes$arquivo[i])
  if (is.na(f)) { message("  ", testes$cod[i], ": arquivo não encontrado"); next }
  d <- fread(f)
  if (testes$cod[i] == "N15") d <- d[startsWith(modelo, "3")]
  if (!nrow(d) || !all(sec %in% names(d))) next
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(d, j = cc, value = as.integer(d[[cc]]))
  flags[[testes$cod[i]]] <- unique(d[, ..sec])[, bit := testes$bit[i]]
}
flags <- rbindlist(flags)
flags <- unique(flags)[, .(flags = sum(bitwShiftL(1L, bit))), by = sec]
testes[, sinalizadas := sapply(bit, function(b) sum(bitwAnd(flags$flags, bitwShiftL(1L, b)) > 0))]
print(testes[, .(cod, nome, sinalizadas)])

# ---- Votação (dois turnos) -----------------------------------------------------
etapa("Votação")
pres <- ler_presidente()
lg <- largo_finalistas(pres, cand_a, cand_b); rm(pres); gc(verbose = FALSE)
for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(lg, j = cc, value = as.integer(lg[[cc]]))
v <- dcast(lg, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ NR_TURNO,
           value.var = c("lula", "flavio", "validos", "total"))
nomes <- unique(lg[, .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)], by = c("SG_UF", "CD_MUNICIPIO"))

# ---- Perfil TSE e Censo por seção ------------------------------------------------
etapa("Perfil TSE e Censo")
arq_perf <- file.path(pasta_extra, "perfil_agregado_secao_n15.csv")
if (!file.exists(arq_perf)) arq_perf <- sub("\\.csv$", "_teste.csv", arq_perf)   # modo de teste do 13
perfil <- fread(arq_perf)
censo <- fread(file.path(pasta_extra, "n15_secoes_censo.csv"), colClasses = list(character = "CD_SETOR"))
d <- merge(v, perfil, by = sec, all.x = TRUE)
d <- merge(d, censo, by = sec, all.x = TRUE)
d <- merge(d, flags, by = sec, all.x = TRUE)
d[is.na(flags), flags := 0L]
d <- merge(d, nomes, by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)
message("  Seções: ", nrow(d), " | com perfil: ", d[!is.na(aptos_perfil), .N], " | com Censo: ", d[!is.na(r_preta_parda), .N],
        " | sinalizadas em algum teste: ", d[flags > 0, .N])

territorios <- c("urbano (demais)", "rural", "favela ou comunidade urbana", "agrupamento quilombola",
                 "agrupamento indígena", "sem ligação válida")
d[is.na(territorio), territorio := "sem ligação válida"]
d[, terr := match(territorio, territorios) - 1L]
d[, renda := exp(r_log_renda) - 1]

# ---- Resumo por UF e Brasil ------------------------------------------------------
etapa("Resumo por UF")
resumir <- function(x) {
  w <- x$aptos_perfil; w[is.na(w)] <- 0
  wm <- function(v) { ok <- !is.na(v) & w > 0; if (!any(ok)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok]) }
  sp <- function(col) sum(x[[col]], na.rm = TRUE)
  out <- list(
    secoes = nrow(x), municipios = uniqueN(x$CD_MUNICIPIO), eleitores = sp("aptos_perfil"),
    comparec_1 = sp("total_1"), comparec_2 = sp("total_2"),
    lula_1 = sp("lula_1"), flavio_1 = sp("flavio_1"), validos_1 = sp("validos_1"),
    lula_2 = sp("lula_2"), flavio_2 = sp("flavio_2"), validos_2 = sp("validos_2"),
    tse = list(fem = sp("p_fem") / sp("aptos_perfil"), i16_24 = sp("p_16_24") / sp("aptos_perfil"),
               i60 = sp("p_60m") / sp("aptos_perfil"), esc_sup = sp("p_esc_sup") / sp("aptos_perfil"),
               esc_baixa = sp("p_esc_baixa") / sp("aptos_perfil"), raca_inf = sp("raca_inf") / sp("aptos_perfil"),
               raca_pp = razao(sp("raca_pp"), sp("raca_inf"))),
    censo = list(renda = wm(x$renda), preta_parda = wm(x$r_preta_parda), indigena = wm(x$r_indigena),
                 analf = wm(x$r_analf), esgoto = wm(x$r_esgoto)),
    territorio = as.list(setNames(sapply(territorios, function(t) sum(x$territorio == t)), territorios)),
    sinalizadas = as.list(setNames(sapply(testes$bit, function(b) sum(bitwAnd(x$flags, bitwShiftL(1L, b)) > 0)), testes$cod)),
    sinalizadas_qualquer = sum(x$flags > 0))
  out
}
ufs <- sort(setdiff(unique(perfil$SG_UF), "ZZ"))
d <- d[SG_UF %in% ufs]
por_uf <- lapply(ufs, function(u) resumir(d[SG_UF == u])); names(por_uf) <- ufs
brasil <- resumir(d)

ler_res <- function(f) { a <- achar(f); if (is.na(a)) NULL else fread(a) }
el_mun <- ler_res("N15_eleitorado_x_populacao_municipios.csv")
if (!is.null(el_mun)) {
  for (u in ufs) {
    e <- el_mun[SG_UF == u]
    por_uf[[u]]$eleitorado_pop15 <- list(mediana = median(e$razao_aptos_pop15), acima_1_2 = sum(e$razao_aptos_pop15 > 1.2),
                                         municipios = nrow(e))
  }
  brasil$eleitorado_pop15 <- list(mediana = median(el_mun$razao_aptos_pop15), acima_1_2 = sum(el_mun$razao_aptos_pop15 > 1.2),
                                  municipios = nrow(el_mun))
}
raca <- ler_res("N15_raca_tse_x_censo.csv")
if (!is.null(raca)) {
  for (u in ufs) if (u %in% raca$SG_UF) por_uf[[u]]$cor_raca_tse_censo <- raca[SG_UF == u, cor_preta_parda]
  brasil$cor_raca_tse_censo <- raca[SG_UF == "BRASIL", cor_preta_parda]
}
resumo <- list(
  gerado_em = format(Sys.time(), "%d/%m/%Y %H:%M"),
  testes = testes[, .(cod, nome, descricao, bit, sinalizadas)],
  territorios = territorios,
  brasil = brasil, ufs = por_uf,
  n15_modelo = ler_res("N15_modelo_resumo.csv"),
  n15_ppc = ler_res("N15_modelo_verificacao_preditiva.csv"),
  n15_territorio_sinalizadas = ler_res("N15_sinalizadas_x_territorio.csv"),
  n15_qualidade = ler_res("N15_qualidade_ligacao.csv"),
  n15_multimodais = ler_res("N15_multimodais_desigualdade.csv"),
  n15_eleitorado_top = if (!is.null(el_mun)) el_mun[order(-razao_aptos_pop15)][1:30, .(SG_UF, NM_MUNICIPIO_IBGE, aptos, pop15, razao_aptos_pop15)])
jsonlite::write_json(resumo, file.path(pasta_painel, "resumo.json"), auto_unbox = TRUE, digits = 5, na = "null")

# ---- Arquivo por UF (colunas) ----------------------------------------------------
etapa("Arquivos por UF")
for (u in ufs) {
  x <- d[SG_UF == u][order(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
  muns <- unique(x[, .(CD_MUNICIPIO, NM_MUNICIPIO)])
  x[, mi := match(CD_MUNICIPIO, muns$CD_MUNICIPIO) - 1L]
  x[, local_chave := paste(CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO)]
  locs <- unique(x[, .(local_chave, NM_LOCAL_VOTACAO, mi, terr, renda, r_preta_parda, r_indigena, r_analf,
                       r_esgoto, r_log_dens, CD_SETOR)], by = "local_chave")
  x[, li := match(local_chave, locs$local_chave) - 1L]
  a <- x$aptos_perfil
  saida <- list(
    uf = u,
    municipios = list(cod = muns$CD_MUNICIPIO, nome = muns$NM_MUNICIPIO),
    locais = list(nome = locs$NM_LOCAL_VOTACAO, mun = locs$mi, terr = locs$terr,
                  renda = as.integer(round(locs$renda)), pp = ri(locs$r_preta_parda), ind = ri(locs$r_indigena),
                  analf = ri(locs$r_analf), esg = ri(locs$r_esgoto),
                  dens = as.integer(round(exp(locs$r_log_dens) - 1)), setor = locs$CD_SETOR),
    secoes = list(mun = x$mi, zona = x$NR_ZONA, secao = x$NR_SECAO, local = x$li, aptos = a,
                  l1 = x$lula_1, f1 = x$flavio_1, v1 = x$validos_1, t1 = x$total_1,
                  l2 = x$lula_2, f2 = x$flavio_2, t2 = x$total_2,
                  fem = ri(x$p_fem / a), i1624 = ri(x$p_16_24 / a), i60 = ri(x$p_60m / a),
                  sup = ri(x$p_esc_sup / a), baixa = ri(x$p_esc_baixa / a), bio = ri(x$p_bio / a),
                  rinf = ri(x$raca_inf / a), rpp = ri(razao(x$raca_pp, x$raca_inf)),
                  ztse = round(x$z_tse, 2), zcen = round(x$z_censo, 2),
                  pcen = ri(x$p_censo), flags = x$flags))
  arq <- file.path(pasta_painel, sprintf("uf_%s.json", tolower(u)))
  jsonlite::write_json(saida, arq, auto_unbox = TRUE, na = "null", digits = NA)
  message(sprintf("  %s: %d seções, %d locais, %.1f MB", u, nrow(x), nrow(locs), file.size(arq) / 1e6))
}
message("\nConcluído. Arquivos em ", normalizePath(pasta_painel))

# =============================================================================
# 13_enriquecimento_ibge.R - Novo teste 15 (N15): enriquecimento com o Censo 2022
#
# Liga cada seção eleitoral ao Censo Demográfico 2022 do IBGE pelo local de
# votação (latitude e longitude do TSE) e usa essas informações para:
#
#   A. Reestimar o N7 (modelo de perfil) com covariáveis do território: renda
#      do responsável, cor ou raça, analfabetismo, esgoto, situação rural e
#      tipo do setor (favela, agrupamento indígena, agrupamento quilombola).
#      O N7 usa efeito fixo de município; por isso só variáveis que mudam
#      DENTRO do município (setor censitário) acrescentam informação.
#      Três modelos, na MESMA amostra de seções:
#        (1) só perfil do TSE (como no 07);
#        (2) perfil do TSE + setor censitário onde fica o local de votação;
#        (3) perfil do TSE + setores num raio de 'raio_m' metros do local
#            (o eleitor não mora necessariamente no setor da escola).
#      Compara R² dentro do município, superdispersão, verificação preditiva
#      e número de seções sinalizadas.
#   B. Território das seções sinalizadas nos testes T1, N7, N8 e N11: quantas
#      ficam em agrupamento indígena, quilombola, favela ou área rural, em
#      comparação com o conjunto das seções.
#   C. Eleitorado (TSE 2026) x população de 15 anos ou mais (Censo 2022) por
#      município: razões acima de 1 indicam cadastro com eleitores que não
#      moram mais no município (mudança, óbito ainda não baixado).
#   D. Cor ou raça declarada ao TSE x Censo: o perfil por seção do TSE traz
#      DS_RACA_COR, mas a maioria dos eleitores não informou. Mede a cobertura
#      e a concordância com o Censo no mesmo local (validação da ligação).
#   E. Desigualdade de renda dentro dos municípios multimodais do N12.
#
# Fontes (todas abertas):
#   IBGE - Censo 2022, Agregados por Setores Censitários (básico, cor ou raça,
#          alfabetização, características do domicílio 2, renda do responsável)
#          e malha de setores (BR_setores_CD2022.gpkg).
#   TSE  - votação por seção 2026, perfil do eleitorado por seção, locais de
#          votação (coordenadas) e correspondência de códigos TSE x IBGE.
#
# Reaproveita os arquivos já baixados pelos scripts 01 e 06; baixa o que faltar.
# No GitHub Actions (variável CI=true) apaga os arquivos brutos grandes depois
# de usar, para caber no disco da máquina.
#
# Saídas: resultados/N15_*.csv
# Espaço: ~6 GB a mais (IBGE) além do que 01 e 06 já usam.
# =============================================================================
source("00_config.R")
for (p in c("sf", "zip")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)          # cálculos planos em projeção métrica (EPSG:5880)
set.seed(2026)

# ---- Parâmetros (ajuste aqui) ---------------------------------------------------
cand_a <- 13L; cand_b <- 22L  # Lula e Flávio Bolsonaro
raio_m    <- 1000             # raio (m) em volta do local de votação (modelo 3)
min_votos <- 50               # seções menores ficam fora do modelo (como no 07)
alfa_fdr  <- 0.001            # q-valor máximo para sinalizar seção (como no 07)
n_rep     <- 200              # simulações da verificação preditiva
min_inf_raca <- 30            # eleitores com raça informada no local (teste D)
apagar_brutos <- identical(Sys.getenv("CI"), "true")
ufs_teste <- toupper(trimws(strsplit(Sys.getenv("UFS_TESTE"), ",")[[1]]))
ufs_uso   <- if (length(ufs_teste) && any(nzchar(ufs_teste))) ufs_teste else setdiff(ufs_esperadas, "ZZ")
if (!identical(ufs_uso, setdiff(ufs_esperadas, "ZZ")))
  message("MODO DE TESTE: só as UFs ", paste(ufs_uso, collapse = ", "))

pasta_ibge <- file.path(pasta_base, "ibge")
dir.create(pasta_ibge, recursive = TRUE, showWarnings = FALSE)
url_censo <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/"
url_setor <- paste0(url_censo, "Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/")
url_renda <- paste0(url_censo, "Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/")
url_malha <- paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/",
                    "malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022/setores/gpkg/BR/")
url_odsele <- "https://cdn.tse.jus.br/estatistica/sead/odsele/"

# Número no formato do IBGE/TSE ("0,53" ou "2453.03"; "X" = sigilo) -> numérico
num_br <- function(x) {
  x <- trimws(as.character(x))
  com_virgula <- grepl(",", x, fixed = TRUE)
  x[com_virgula] <- gsub(",", ".", gsub(".", "", x[com_virgula], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
apagar <- function(...) if (apagar_brutos) unlink(c(...), recursive = TRUE)
razao <- function(a, b) ifelse(b > 0, a / b, NA_real_)

# Encontra na página do IBGE o arquivo mais recente que começa com 'prefixo'
# (o IBGE troca a data no nome quando atualiza), baixa e extrai o CSV.
obter_ibge <- function(pasta_url, prefixo) {
  html <- paste(readLines(pasta_url, warn = FALSE), collapse = " ")
  html <- iconv(html, "latin1", "ASCII", sub = "")    # a página vem em Latin-1
  hrefs <- gsub('^href="|"$', "", regmatches(html, gregexpr('href="[^"]+"', html, useBytes = TRUE))[[1]])
  alvo <- hrefs[startsWith(hrefs, prefixo) & grepl("\\.zip$", hrefs) & !grepl("xlsx", hrefs)]
  if (!length(alvo)) stop("Não achei '", prefixo, "' em ", pasta_url)
  alvo <- sort(alvo, decreasing = TRUE)[1]
  z <- file.path(pasta_ibge, alvo)
  if (!baixar_zip(paste0(pasta_url, alvo), z)) stop("Falhou o download de ", alvo)
  if (!extrair_zip(z, pasta_ibge, "\\.csv$")) stop("Falhou a extração de ", alvo)
  lst <- zip::zip_list(z)$filename
  list(csv = file.path(pasta_ibge, lst[grepl("\\.csv$", lst)][1]), zip = z)
}

# Lê só as colunas pedidas de um CSV do IBGE (nomes sem diferença de maiúsculas)
ler_ibge <- function(arq, cols, texto = c("CD_SETOR", "SITUACAO")) {
  cab <- names(fread(arq, sep = ";", nrows = 0, encoding = "Latin-1"))
  cab_n <- toupper(gsub("[^A-Za-z0-9_]", "", cab))   # tira BOM e aspas
  if (!"CD_SETOR" %in% cab_n && length(grep("SETOR", cab_n))) cab_n[grep("SETOR", cab_n)[1]] <- "CD_SETOR"
  falta <- setdiff(cols, cab_n)
  if (length(falta)) stop(basename(arq), " sem as colunas: ", paste(falta, collapse = ", "),
                          "\n  Colunas do arquivo (início): ", paste(head(cab, 15), collapse = ", "))
  dt <- fread(arq, sep = ";", select = which(cab_n %in% cols), colClasses = "character",
              encoding = "Latin-1", na.strings = c("X", "x", ".", ""), showProgress = FALSE)
  setnames(dt, cab_n[cab_n %in% cols])
  for (cc in setdiff(names(dt), texto)) set(dt, j = cc, value = num_br(dt[[cc]]))
  dt
}

# =============================================================================
# 1. Censo 2022 por setor censitário
# =============================================================================
etapa("1. Censo 2022 - agregados por setor")
arq_setor <- file.path(pasta_ibge, "setores_censo2022_resumo.csv")
if (file.exists(arq_setor)) {
  setor <- fread(arq_setor, colClasses = list(character = "CD_SETOR"))
  message("  Já preparado: ", nrow(setor), " setores")
} else {
  fontes <- list(
    basico = list(url_setor, "Agregados_por_setores_basico_BR",
                  c("CD_SETOR", "SITUACAO", "CD_SIT", "CD_TIPO", "AREA_KM2", "CD_MUN", "CD_RGI", "V0001")),
    cor    = list(url_setor, "Agregados_por_setores_cor_ou_raca_BR",
                  c("CD_SETOR", sprintf("V%05d", 1317:1321))),
    alfab  = list(url_setor, "Agregados_por_setores_alfabetizacao_BR", c("CD_SETOR", "V00900", "V00901")),
    dom2   = list(url_setor, "Agregados_por_setores_caracteristicas_domicilio2_BR",
                  c("CD_SETOR", sprintf("V%05d", 309:316))),
    renda  = list(url_renda, "Agregados_por_setores_renda_responsavel_BR", c("CD_SETOR", "V06001", "V06004")))
  partes <- list()
  for (nm in names(fontes)) {
    f <- fontes[[nm]]
    message("  ", nm)
    a <- obter_ibge(f[[1]], f[[2]])
    partes[[nm]] <- ler_ibge(a$csv, f[[3]])
    message(sprintf("    %d setores; %d com sigilo/ausente em alguma coluna",
                    nrow(partes[[nm]]), sum(!complete.cases(partes[[nm]]))))
    apagar(a$csv, a$zip)
  }
  setor <- Reduce(function(a, b) merge(a, b, by = "CD_SETOR", all.x = TRUE), partes)
  # "X" = célula com sigilo (contagem pequena). Nas somas por categoria vira 0;
  # o setor só fica sem valor se todas as categorias estiverem com sigilo.
  soma <- function(...) { m <- cbind(...); r <- rowSums(m, na.rm = TRUE); r[rowSums(!is.na(m)) == 0] <- NA; r }
  setor[, `:=`(
    pop = V0001,
    cor_tot = soma(V01317, V01318, V01319, V01320, V01321),
    preta_parda = soma(V01318, V01320), indigena = fcoalesce(V01321, 0),
    alf15 = soma(V00900, V00901), analf15 = fcoalesce(V00901, 0),
    esg_tot = soma(V00309, V00310, V00311, V00312, V00313, V00314, V00315, V00316),
    esg_rede = fcoalesce(V00309, 0),
    resp = V06001, renda_soma = V06001 * V06004)]   # soma aproximada: média x responsáveis
  setor[is.na(cor_tot), c("preta_parda", "indigena") := NA_real_]
  setor[is.na(alf15), analf15 := NA_real_]
  setor[is.na(esg_tot), esg_rede := NA_real_]
  setor <- setor[, .(CD_SETOR, SITUACAO, CD_SIT, CD_TIPO, AREA_KM2, CD_MUN, CD_RGI, pop, cor_tot,
                     preta_parda, indigena, alf15, analf15, esg_tot, esg_rede, resp, renda_soma)]
  fwrite(setor, arq_setor)
}
setor[, CD_MUN := as.integer(CD_MUN)]
message(sprintf("  Setores: %d | população: %s | com renda: %d | com cor/raça: %d",
                nrow(setor), format(sum(setor$pop, na.rm = TRUE), big.mark = "."),
                sum(!is.na(setor$renda_soma)), sum(!is.na(setor$cor_tot))))

# indicadores do próprio setor (para o modelo 2)
setor[, `:=`(
  s_log_renda = log(razao(renda_soma, resp) + 1),
  s_preta_parda = razao(preta_parda, cor_tot),
  s_indigena = razao(indigena, cor_tot),
  s_analf = razao(analf15, alf15),
  s_esgoto = razao(esg_rede, esg_tot),
  s_log_dens = log(razao(pop, AREA_KM2) + 1),
  rural = as.integer(CD_SIT >= 5),
  favela = as.integer(CD_TIPO == 1),
  agr_indigena = as.integer(CD_TIPO == 5),
  agr_quilombola = as.integer(CD_TIPO == 9))]

# =============================================================================
# 2. Locais de votação, correspondência de municípios e malha de setores
# =============================================================================
etapa("2. Locais de votação e correspondência TSE x IBGE")
pasta_loc <- file.path(pasta_extra, "locais")
arqs_loc <- list.files(pasta_loc, "\\.csv$", full.names = TRUE)
if (!length(arqs_loc)) {
  z <- file.path(pasta_extra, "zip", sprintf("eleitorado_local_votacao_%d.zip", ano))
  dir.create(dirname(z), recursive = TRUE, showWarnings = FALSE)
  if (!baixar_zip(sprintf("%seleitorado_locais_votacao/eleitorado_local_votacao_%d.zip", url_odsele, ano), z))
    stop("Falhou o download dos locais de votação.")
  extrair_zip(z, pasta_loc, "BRASIL\\.csv$")
  apagar(z)
  arqs_loc <- list.files(pasta_loc, "\\.csv$", full.names = TRUE)
}
if (any(grepl("BRASIL", arqs_loc))) arqs_loc <- arqs_loc[grepl("BRASIL", arqs_loc)]
loc <- rbindlist(lapply(arqs_loc, ler_csv_tse,
                        colunas = c("NR_TURNO", sec, "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO",
                                    "NR_LATITUDE", "NR_LONGITUDE")), fill = TRUE)
for (cc in c("NR_TURNO", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO"))
  set(loc, j = cc, value = as.integer(loc[[cc]]))
loc <- unique(loc[SG_UF %in% ufs_uso], by = c("NR_TURNO", sec))
loc[, `:=`(lat = num_br(NR_LATITUDE), lon = num_br(NR_LONGITUDE))]
loc[!(lat %between% c(-34, 6) & lon %between% c(-74.5, -28.5)), c("lat", "lon") := NA_real_]
message(sprintf("  Seções x turno: %d | sem coordenada válida: %d (%.2f%%)",
                nrow(loc), loc[is.na(lat), .N], 100 * loc[is.na(lat), .N] / nrow(loc)))

z_mun <- file.path(pasta_extra, "zip", "municipio_tse_ibge.zip")
dir.create(dirname(z_mun), recursive = TRUE, showWarnings = FALSE)
if (!baixar_zip(paste0(url_odsele, "municipio_tse_ibge/municipio_tse_ibge.zip"), z_mun))
  stop("Falhou o download da correspondência TSE x IBGE.")
extrair_zip(z_mun, pasta_extra, "\\.csv$")
mun <- ler_csv_tse(file.path(pasta_extra, "municipio_tse_ibge.csv"),
                   c("SG_UF", "CD_MUNICIPIO_TSE", "CD_MUNICIPIO_IBGE", "NM_MUNICIPIO_IBGE"))
mun[, `:=`(CD_MUNICIPIO = as.integer(CD_MUNICIPIO_TSE), CD_MUN = as.integer(CD_MUNICIPIO_IBGE))]
mun <- unique(mun[, .(SG_UF, CD_MUNICIPIO, CD_MUN, NM_MUNICIPIO_IBGE)], by = c("SG_UF", "CD_MUNICIPIO"))
loc <- merge(loc, mun[, .(SG_UF, CD_MUNICIPIO, CD_MUN)], by = c("SG_UF", "CD_MUNICIPIO"), all.x = TRUE)
message("  Seções sem correspondência de município: ", loc[is.na(CD_MUN), .N])

# um ponto por local de votação (mesmas coordenadas nos dois turnos, em geral)
pontos <- unique(loc[!is.na(lat), .(SG_UF, CD_MUNICIPIO, CD_MUN, NR_ZONA, NR_LOCAL_VOTACAO, lat, lon)])
pontos[, id_ponto := .I]
loc <- merge(loc, pontos, by = c("SG_UF", "CD_MUNICIPIO", "CD_MUN", "NR_ZONA", "NR_LOCAL_VOTACAO", "lat", "lon"),
             all.x = TRUE)
message("  Locais distintos com coordenada: ", nrow(pontos))

etapa("2b. Malha de setores e ligação ponto -> setor")
arq_gpkg <- file.path(pasta_ibge, "BR_setores_CD2022.gpkg")
if (!file.exists(arq_gpkg)) {
  message("Baixando BR_setores_CD2022.gpkg (malha de setores) ...")
  tmp <- paste0(arq_gpkg, ".parcial")
  download.file(paste0(url_malha, "BR_setores_CD2022.gpkg"), tmp, mode = "wb", quiet = TRUE)
  file.rename(tmp, arq_gpkg)
}
camada <- st_layers(arq_gpkg)$name[1]
cod_uf <- c(RO=11, AC=12, AM=13, RR=14, PA=15, AP=16, TO=17, MA=21, PI=22, CE=23, RN=24, PB=25, PE=26,
            AL=27, SE=28, BA=29, MG=31, ES=32, RJ=33, SP=35, PR=41, SC=42, RS=43, MS=50, MT=51, GO=52, DF=53)

liga_setor <- list(); liga_raio <- list()
for (uf in ufs_uso) {
  p_uf <- pontos[SG_UF == uf]
  if (!nrow(p_uf)) next
  q <- sprintf("SELECT * FROM \"%s\" WHERE substr(CAST(CD_SETOR AS TEXT), 1, 2) = '%d'", camada, cod_uf[[uf]])
  s_uf <- st_read(arq_gpkg, query = q, quiet = TRUE)
  s_uf <- st_transform(s_uf[, "CD_SETOR"], 5880)
  s_uf$CD_SETOR <- as.character(s_uf$CD_SETOR)
  pts <- st_transform(st_as_sf(p_uf, coords = c("lon", "lat"), crs = 4674), 5880)
  dentro <- tryCatch(st_intersects(pts, s_uf),
                     error = function(e) { s_uf <<- st_make_valid(s_uf); st_intersects(pts, s_uf) })
  idx <- vapply(dentro, function(v) if (length(v)) v[1] else NA_integer_, integer(1))
  liga_setor[[uf]] <- data.table(id_ponto = p_uf$id_ponto, CD_SETOR = s_uf$CD_SETOR[idx])
  cent <- suppressWarnings(st_point_on_surface(s_uf))
  viz <- st_is_within_distance(pts, cent, dist = raio_m)
  lr <- data.table(id_ponto = rep(p_uf$id_ponto, lengths(viz)), CD_SETOR = s_uf$CD_SETOR[unlist(viz)])
  lr <- unique(rbind(lr, liga_setor[[uf]][!is.na(CD_SETOR)]))   # inclui sempre o setor do próprio local
  liga_raio[[uf]] <- lr
  message(sprintf("  %s: %d locais, %d setores; sem setor: %d; setores por local no raio (mediana): %d",
                  uf, nrow(p_uf), nrow(s_uf), sum(is.na(idx)), as.integer(median(lengths(viz)) + 1)))
  rm(s_uf, cent, viz, pts); gc(verbose = FALSE)
}
liga_setor <- rbindlist(liga_setor); liga_raio <- rbindlist(liga_raio)

# conferência: o setor encontrado é do mesmo município do local de votação?
liga_setor <- merge(liga_setor, pontos[, .(id_ponto, CD_MUN)], by = "id_ponto")
liga_setor[, mun_setor := as.integer(substr(CD_SETOR, 1, 7))]
liga_setor[, coord_ok := !is.na(CD_SETOR) & mun_setor == CD_MUN]
qual <- liga_setor[, .(locais = .N, sem_setor = sum(is.na(CD_SETOR)),
                       outro_municipio = sum(!is.na(CD_SETOR) & !coord_ok), ok = sum(coord_ok))]
print(qual)

# covariáveis por local: setor do local (modelo 2) e raio (modelo 3)
cov_setor <- merge(liga_setor[coord_ok == TRUE, .(id_ponto, CD_SETOR)],
                   setor[, .(CD_SETOR, s_log_renda, s_preta_parda, s_indigena, s_analf, s_esgoto, s_log_dens,
                             rural, favela, agr_indigena, agr_quilombola, CD_TIPO, CD_SIT)],
                   by = "CD_SETOR")
cov_raio <- merge(liga_raio[id_ponto %in% liga_setor[coord_ok == TRUE, id_ponto]], setor, by = "CD_SETOR")
cov_raio <- cov_raio[, .(r_log_renda = log(razao(sum(renda_soma, na.rm = TRUE), sum(resp[!is.na(renda_soma)], na.rm = TRUE)) + 1),
                         r_preta_parda = razao(sum(preta_parda, na.rm = TRUE), sum(cor_tot, na.rm = TRUE)),
                         r_indigena = razao(sum(indigena, na.rm = TRUE), sum(cor_tot, na.rm = TRUE)),
                         r_analf = razao(sum(analf15, na.rm = TRUE), sum(alf15, na.rm = TRUE)),
                         r_esgoto = razao(sum(esg_rede, na.rm = TRUE), sum(esg_tot, na.rm = TRUE)),
                         r_log_dens = log(razao(sum(pop, na.rm = TRUE), sum(AREA_KM2, na.rm = TRUE)) + 1),
                         r_pop = sum(pop, na.rm = TRUE), r_setores = .N),
                     by = id_ponto]
loc <- merge(loc, cov_setor, by = "id_ponto", all.x = TRUE)
loc <- merge(loc, cov_raio, by = "id_ponto", all.x = TRUE)
loc[, territorio := fcase(is.na(CD_SETOR), "sem ligação válida",
                          agr_indigena == 1, "agrupamento indígena",
                          agr_quilombola == 1, "agrupamento quilombola",
                          favela == 1, "favela ou comunidade urbana",
                          rural == 1, "rural",
                          default = "urbano (demais)")]
salvar(qual, "N15_qualidade_ligacao.csv")
salvar(loc[NR_TURNO == 2, .(secoes = .N), by = territorio][, pct := round(100 * secoes / sum(secoes), 2)][order(-secoes)],
       "N15_territorio_secoes.csv")
print(loc[NR_TURNO == 2, .(secoes = .N), by = territorio][order(-secoes)])

# =============================================================================
# 3. Perfil do eleitorado por seção (TSE), com cor ou raça declarada
# =============================================================================
etapa("3. Perfil do eleitorado por seção (com cor ou raça)")
arq_perf_ag <- file.path(pasta_extra, "perfil_agregado_secao_n15.csv")
if (file.exists(arq_perf_ag)) {
  perfil <- fread(arq_perf_ag)
  perfil <- perfil[SG_UF %in% ufs_uso]
} else {
  pasta_perf <- file.path(pasta_extra, "perfil")
  dir.create(pasta_perf, recursive = TRUE, showWarnings = FALSE)
  col_p <- c(sec, "DS_GENERO", "DS_FAIXA_ETARIA", "DS_GRAU_ESCOLARIDADE", "DS_RACA_COR",
             "QT_ELEITORES", "QT_ELEITORES_PERFIL", "QT_ELEITORES_BIOMETRIA")
  sem_acento <- function(x) toupper(chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇáàâãäéèêëíìîïóòôõöúùûüç",
                                           "AAAAAEEEEIIIIOOOOOUUUUCaaaaaeeeeiiiiooooouuuuc", x))
  perfil <- list()
  for (uf in ufs_uso) {
    f <- file.path(pasta_perf, sprintf("perfil_eleitor_secao_%d_%s.csv", ano, uf))
    z <- NULL
    if (!file.exists(f)) {
      z <- file.path(pasta_extra, "zip", sprintf("perfil_eleitor_secao_%d_%s.zip", ano, uf))
      if (!baixar_zip(sprintf("%sperfil_eleitor_secao/perfil_eleitor_secao_%d_%s.zip", url_odsele, ano, uf), z))
        stop("Falhou o download do perfil de ", uf)
      extrair_zip(z, pasta_perf, "\\.csv$")
    }
    message("  Lendo ", basename(f))
    p <- ler_csv_tse(f, col_p)
    if (!"QT_ELEITORES_PERFIL" %in% names(p)) setnames(p, "QT_ELEITORES", "QT_ELEITORES_PERFIL")
    if (!"DS_RACA_COR" %in% names(p)) p[, DS_RACA_COR := NA_character_]
    p[, `:=`(gen = sem_acento(DS_GENERO), esc = sem_acento(DS_GRAU_ESCOLARIDADE), raca = sem_acento(DS_RACA_COR),
             idade = suppressWarnings(as.integer(sub("^\\D*(\\d+).*$", "\\1", DS_FAIXA_ETARIA))),
             q = as.numeric(QT_ELEITORES_PERFIL))]
    perfil[[uf]] <- p[, .(aptos_perfil = sum(q),
                          p_fem = sum(q[gen %like% "FEMININO"]),
                          p_16_24 = sum(q[!is.na(idade) & idade < 25]),
                          p_45_59 = sum(q[!is.na(idade) & idade >= 45 & idade < 60]),
                          p_60m = sum(q[!is.na(idade) & idade >= 60]),
                          p_esc_baixa = sum(q[esc %like% "ANALFABETO|LE E ESCREVE|FUNDAMENTAL INCOMPLETO"]),
                          p_esc_sup = sum(q[esc %like% "SUPERIOR"]),
                          p_bio = sum(as.numeric(QT_ELEITORES_BIOMETRIA), na.rm = TRUE),
                          raca_inf = sum(q[!is.na(raca) & !raca %like% "NAO INFORMADO|^$"]),
                          raca_pp = sum(q[raca %in% c("PRETA", "PARDA")]),
                          raca_ind = sum(q[raca %like% "INDIGENA"])),
                      by = sec]
    rm(p); gc(verbose = FALSE)
    apagar(f, z)
  }
  perfil <- rbindlist(perfil)
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(perfil, j = cc, value = as.integer(perfil[[cc]]))
  # guarda a versão completa; no modo de teste, num arquivo à parte
  fwrite(perfil, if (identical(ufs_uso, setdiff(ufs_esperadas, "ZZ"))) arq_perf_ag else sub("\\.csv$", "_teste.csv", arq_perf_ag))
}
vars_tse <- c("p_fem", "p_16_24", "p_45_59", "p_60m", "p_esc_baixa", "p_esc_sup", "p_bio")
for (v in vars_tse) set(perfil, j = v, value = perfil[[v]] / perfil$aptos_perfil)
message(sprintf("  Perfil: %d seções | eleitores: %s | com cor/raça informada: %.1f%%",
                nrow(perfil), format(sum(perfil$aptos_perfil), big.mark = "."),
                100 * sum(perfil$raca_inf) / sum(perfil$aptos_perfil)))

# =============================================================================
# 4. Votação por seção (Presidente)
# =============================================================================
etapa("4. Votação por seção (Presidente)")
if (!file.exists(arquivo_br)) {
  z <- file.path(pasta_zip, sprintf("votacao_secao_%d_BR.zip", ano))
  if (!baixar_zip(paste0(url_base_votacao, basename(z)), z)) stop("Falhou o download da votação.")
  extrair_zip(z, pasta_csv)
}
pres <- ler_presidente()
lg <- largo_finalistas(pres, cand_a, cand_b)
rm(pres); gc(verbose = FALSE)
for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(lg, j = cc, value = as.integer(lg[[cc]]))
lg <- lg[SG_UF %in% ufs_uso]
lg[, `:=`(mun = paste(SG_UF, CD_MUNICIPIO), n2 = lula + flavio)]
apagar(file.path(pasta_zip, sprintf("votacao_secao_%d_BR.zip", ano)))

# =============================================================================
# 5. A - N7 reestimado com o Censo
# =============================================================================
etapa("5. A - Modelo de perfil (N7) com e sem o Censo 2022")
rbetabinom <- function(n, p, rho) {
  if (rho <= 1e-8) return(rbinom(length(n), n, p))
  a <- p * (1 / rho - 1); b <- (1 - p) * (1 / rho - 1)
  rbinom(length(n), n, rbeta(length(n), a, b))
}
ajustar <- function(x, vars, modelo, alvo) {
  x <- copy(x)
  coefs <- fe_lm(x$ly, x[, ..vars], fe = x$mun, cl = x$mun, w = x$n)
  b <- setNames(coefs$coef, coefs$termo)
  x[, xb := (as.matrix(x[, names(b), with = FALSE]) %*% b)[, 1]]
  x[, alfa := sum((ly - xb) * n) / sum(n), by = mun]
  x[, p_hat := plogis(xb + alfa)]
  x[, r2 := (y - n * p_hat)^2 / (n * p_hat * (1 - p_hat))]
  rho <- max(0, sum((x$r2 - 1) * (x$n - 1)) / sum((x$n - 1)^2))
  x[, z := (y - n * p_hat) / sqrt(n * p_hat * (1 - p_hat) * (1 + (n - 1) * rho))]
  x[, q := bh(2 * pnorm(-abs(z)))]
  media_mun <- ave(x$ly * x$n, x$mun, FUN = sum) / ave(x$n, x$mun, FUN = sum)
  r2w <- 1 - sum(x$n * (x$ly - x$xb - x$alfa)^2) / sum(x$n * (x$ly - media_mun)^2)
  est <- function(yv) {
    s <- yv / x$n
    c(secoes_com_zero = sum(yv == 0), secoes_acima_90pct = sum(s > 0.9), secoes_abaixo_10pct = sum(s < 0.1),
      dp_dentro_municipio = sqrt(sum(x$n * (s - ave(s * x$n, x$mun, FUN = sum) / ave(x$n, x$mun, FUN = sum))^2) / sum(x$n)))
  }
  obs <- est(x$y)
  sim <- t(replicate(n_rep, est(rbetabinom(x$n, x$p_hat, rho))))
  ppc <- data.table(alvo = alvo, modelo = modelo, estatistica = names(obs), observado = obs,
                    simulado_media = colMeans(sim),
                    simulado_p2.5 = apply(sim, 2, quantile, 0.025), simulado_p97.5 = apply(sim, 2, quantile, 0.975),
                    p_preditivo = sapply(seq_along(obs), function(i)
                      min(1, 2 * min(mean(sim[, i] >= obs[i]), mean(sim[, i] <= obs[i])))))
  resumo <- data.table(alvo = alvo, modelo = modelo, secoes = nrow(x), covariaveis = length(vars),
                       rho = rho, r2_dentro_municipio = r2w,
                       pct_absz_maior_3 = 100 * mean(abs(x$z) > 3), sinalizadas = sum(x$q < alfa_fdr))
  coefs[, `:=`(alvo = alvo, modelo = modelo)]
  message(sprintf("    %-28s R² dentro = %.3f | rho = %.4f | |z|>3: %.2f%% | sinalizadas: %d | >90%%: obs %d x sim %.0f",
                  modelo, r2w, rho, resumo$pct_absz_maior_3, resumo$sinalizadas, obs[["secoes_acima_90pct"]],
                  ppc[estatistica == "secoes_acima_90pct", simulado_media]))
  list(coefs = coefs, ppc = ppc, resumo = resumo, z = x[, c(sec, "z", "p_hat"), with = FALSE],
       flag = x[q < alfa_fdr, c("NR_TURNO", sec, "z", "q", "territorio"), with = FALSE][, `:=`(alvo = alvo, modelo = modelo)])
}

vars_setor <- c("s_log_renda", "s_preta_parda", "s_indigena", "s_analf", "s_esgoto", "s_log_dens",
                "rural", "favela", "agr_indigena", "agr_quilombola")
vars_raio  <- c("r_log_renda", "r_preta_parda", "r_indigena", "r_analf", "r_esgoto", "r_log_dens",
                "rural", "favela", "agr_indigena", "agr_quilombola")
alvos <- list(list(turno = 2L, cand = cand_a, rot = "Lula (2º turno, % dos 2 finalistas)"),
              list(turno = 1L, cand = cand_a, rot = "Lula (1º turno, % dos válidos)"),
              list(turno = 1L, cand = cand_b, rot = "Flávio (1º turno, % dos válidos)"))
res_a <- list()
for (a in alvos) {
  x <- merge(lg[NR_TURNO == a$turno], perfil, by = sec)
  x <- merge(x, loc[NR_TURNO == a$turno, c(sec, "territorio", unique(c(vars_setor, vars_raio))), with = FALSE],
             by = sec, all.x = TRUE)
  x[, y := if (a$cand == cand_a) lula else flavio]
  x[, n := if (a$turno == 2L) n2 else validos]
  n_antes <- x[n >= min_votos & complete.cases(x[, ..vars_tse]), .N]
  x <- x[n >= min_votos & complete.cases(x[, unique(c(vars_tse, vars_setor, vars_raio)), with = FALSE])]
  message(sprintf("  %s: %d seções com todas as covariáveis (de %d no N7; %.1f%%)",
                  a$rot, nrow(x), n_antes, 100 * nrow(x) / n_antes))
  x[, ly := logit_emp(y, n)]
  res_a[[length(res_a) + 1]] <- list(
    ajustar(x, vars_tse, "1 perfil TSE", a$rot),
    ajustar(x, c(vars_tse, vars_setor), "2 TSE + setor do local", a$rot),
    ajustar(x, c(vars_tse, vars_raio), sprintf("3 TSE + raio %d m", raio_m), a$rot))
}
res_a <- unlist(res_a, recursive = FALSE)
resumo_a <- rbindlist(lapply(res_a, `[[`, "resumo"))
ppc_a <- rbindlist(lapply(res_a, `[[`, "ppc"))
coef_a <- rbindlist(lapply(res_a, `[[`, "coefs"))
flag_a <- rbindlist(lapply(res_a, `[[`, "flag"))
print(resumo_a[, .(alvo, modelo, secoes, rho = round(rho, 4), r2 = round(r2_dentro_municipio, 3),
                   absz3 = round(pct_absz_maior_3, 2), sinalizadas)])
salvar(resumo_a, "N15_modelo_resumo.csv")
salvar(ppc_a, "N15_modelo_verificacao_preditiva.csv")
salvar(coef_a, "N15_modelo_coeficientes.csv")
# o que acontece com as seções sinalizadas pelo modelo só-TSE ao acrescentar o Censo
base_flag <- flag_a[modelo == "1 perfil TSE"]
persist <- rbindlist(lapply(unique(flag_a$modelo[flag_a$modelo != "1 perfil TSE"]), function(m) {
  outro <- flag_a[modelo == m]
  base_flag[, .(modelo_comparado = m, sinalizadas_so_tse = .N,
                continuam = sum(paste(alvo, NR_TURNO, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO) %in%
                                  outro[, paste(alvo, NR_TURNO, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)])), by = alvo]
}))
persist[, pct_continuam := round(100 * continuam / sinalizadas_so_tse, 1)]
print(persist)
salvar(persist, "N15_modelo_persistencia_sinalizadas.csv")
salvar(flag_a[order(modelo, -abs(z))], "N15_modelo_secoes_sinalizadas.csv")

# por seção (usado pelo painel, script 14): z do 2º turno (Lula) sem e com o Censo,
# e o perfil do Censo em volta do local de votação
z1 <- setnames(copy(res_a[[1]]$z), c("z", "p_hat"), c("z_tse", "p_tse"))
z3 <- setnames(copy(res_a[[3]]$z), c("z", "p_hat"), c("z_censo", "p_censo"))
sec_censo <- merge(loc[NR_TURNO == 2, c(sec, "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "lat", "lon", "CD_SETOR",
                                        "territorio", "r_log_renda", "r_preta_parda", "r_indigena", "r_analf",
                                        "r_esgoto", "r_log_dens", "r_pop"), with = FALSE],
                   merge(z1, z3, by = sec, all = TRUE), by = sec, all.x = TRUE)
fwrite(sec_censo, file.path(pasta_extra, "n15_secoes_censo.csv"))
message("  Tabela por seção para o painel: ", nrow(sec_censo), " seções")

# =============================================================================
# 6. B - Território das seções sinalizadas em testes anteriores
# =============================================================================
etapa("6. B - Território das seções sinalizadas (T1, N7, N8, N11)")
achar <- function(f) {
  for (d in c(pasta_saida, file.path("..", "resultados"))) if (file.exists(file.path(d, f))) return(file.path(d, f))
  NA_character_
}
listas <- list(T1 = "T1_secoes_0_ou_100.csv", N7 = "N7_secoes_sinalizadas.csv",
               N8 = "N8_secoes_sinalizadas.csv", N11 = "N11_secoes_sinalizadas.csv")
terr2 <- loc[NR_TURNO == 2, c(sec, "territorio"), with = FALSE]
base_terr <- terr2[, .(pct_todas = 100 * .N / nrow(terr2)), by = territorio]
res_b <- list()
for (nm in names(listas)) {
  f <- achar(listas[[nm]])
  if (is.na(f)) { message("  ", nm, ": arquivo não encontrado, pulado"); next }
  s <- fread(f)
  for (cc in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) set(s, j = cc, value = as.integer(s[[cc]]))
  s <- unique(s[SG_UF %in% ufs_uso, ..sec])
  s <- merge(s, terr2, by = sec, all.x = TRUE)
  s[is.na(territorio), territorio := "sem ligação válida"]
  r <- s[, .(sinalizadas = .N), by = territorio][, pct_sinalizadas := 100 * sinalizadas / sum(sinalizadas)]
  r <- merge(r, base_terr, by = "territorio", all.x = TRUE)
  r[, `:=`(teste = nm, razao_sobre_base = round(pct_sinalizadas / pct_todas, 1))]
  res_b[[nm]] <- r
}
res_b <- rbindlist(res_b)
if (nrow(res_b)) {
  setcolorder(res_b, c("teste", "territorio"))
  res_b[, `:=`(pct_sinalizadas = round(pct_sinalizadas, 2), pct_todas = round(pct_todas, 2))]
  print(res_b[order(teste, -sinalizadas)])
  salvar(res_b[order(teste, -sinalizadas)], "N15_sinalizadas_x_territorio.csv")
}

# =============================================================================
# 7. C - Eleitorado x população por município
# =============================================================================
etapa("7. C - Eleitorado 2026 (TSE) x população de 15 anos ou mais (Censo 2022)")
el_mun <- perfil[, .(aptos = sum(aptos_perfil)), by = .(SG_UF, CD_MUNICIPIO)]
el_mun <- merge(el_mun, mun, by = c("SG_UF", "CD_MUNICIPIO"))
pop_mun <- setor[, .(pop = sum(pop, na.rm = TRUE), pop15 = sum(alf15, na.rm = TRUE)), by = CD_MUN]
el_mun <- merge(el_mun, pop_mun, by = "CD_MUN")
el_mun[, razao_aptos_pop15 := aptos / pop15]
res_c <- el_mun[, .(municipios = .N, mediana = median(razao_aptos_pop15),
                    p95 = quantile(razao_aptos_pop15, 0.95),
                    acima_1 = sum(razao_aptos_pop15 > 1), acima_1.2 = sum(razao_aptos_pop15 > 1.2))]
print(res_c)
salvar(res_c, "N15_eleitorado_x_populacao_resumo.csv")
salvar(el_mun[order(-razao_aptos_pop15)], "N15_eleitorado_x_populacao_municipios.csv")

# =============================================================================
# 8. D - Cor ou raça: TSE x Censo no mesmo local
# =============================================================================
etapa("8. D - Cor ou raça declarada ao TSE x Censo 2022")
rl <- merge(perfil[, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, aptos_perfil, raca_inf, raca_pp, raca_ind)],
            loc[NR_TURNO == 2, .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, id_ponto, r_preta_parda, r_indigena)],
            by = sec)
rl <- rl[!is.na(id_ponto), .(aptos = sum(aptos_perfil), inf = sum(raca_inf), pp = sum(raca_pp), ind = sum(raca_ind),
                              censo_pp = r_preta_parda[1], censo_ind = r_indigena[1], SG_UF = SG_UF[1]), by = id_ponto]
rl[, `:=`(cobertura = inf / aptos, tse_pp = razao(pp, inf), tse_ind = razao(ind, inf))]
ok_d <- rl[inf >= min_inf_raca & !is.na(censo_pp)]
wcor <- function(a, b, w) { ok <- complete.cases(a, b, w); cov.wt(cbind(a[ok], b[ok]), w[ok], cor = TRUE)$cor[1, 2] }
cob_uf <- rl[, .(cobertura_raca_tse = sum(inf) / sum(aptos)), by = SG_UF]
por_uf <- ok_d[, .(locais = .N,
                   cor_preta_parda = if (.N > 10) wcor(tse_pp, censo_pp, inf) else NA_real_,
                   cor_indigena = if (.N > 10) wcor(tse_ind, censo_ind, inf) else NA_real_,
                   media_tse_pp = weighted.mean(tse_pp, inf), media_censo_pp = weighted.mean(censo_pp, inf)),
               by = SG_UF]
res_d <- rbind(
  data.table(SG_UF = "BRASIL", locais = nrow(ok_d), cobertura_raca_tse = sum(rl$inf) / sum(rl$aptos),
             cor_preta_parda = wcor(ok_d$tse_pp, ok_d$censo_pp, ok_d$inf),
             cor_indigena = wcor(ok_d$tse_ind, ok_d$censo_ind, ok_d$inf),
             media_tse_pp = weighted.mean(ok_d$tse_pp, ok_d$inf), media_censo_pp = weighted.mean(ok_d$censo_pp, ok_d$inf)),
  merge(cob_uf, por_uf, by = "SG_UF", all.x = TRUE)[order(SG_UF)], use.names = TRUE)
print(res_d[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
salvar(res_d, "N15_raca_tse_x_censo.csv")

# =============================================================================
# 9. E - Desigualdade de renda nos municípios multimodais (N12)
# =============================================================================
etapa("9. E - Renda dentro dos municípios multimodais do N12")
f12 <- achar("N12_municipios_multimodais.csv")
des <- setor[!is.na(renda_soma) & resp > 0,
             .(cv_renda_setores = { m <- razao(renda_soma, resp); w <- resp
               sqrt(sum(w * (m - sum(w * m) / sum(w))^2) / sum(w)) / (sum(w * m) / sum(w)) },
               setores = .N), by = CD_MUN]
des[, percentil_cv := round(100 * frank(cv_renda_setores) / .N, 1)]
if (!is.na(f12)) {
  m12 <- fread(f12)
  m12 <- m12[ano == 2026 | mm26 == TRUE, .(SG_UF, CD_MUNICIPIO = as.integer(CD_MUNICIPIO), NM_MUNICIPIO, dip)]
  m12 <- unique(merge(m12, mun[, .(SG_UF, CD_MUNICIPIO, CD_MUN)], by = c("SG_UF", "CD_MUNICIPIO")),
                by = c("SG_UF", "CD_MUNICIPIO"))
  m12 <- merge(m12, des, by = "CD_MUN", all.x = TRUE)
  message(sprintf("  CV da renda entre setores: mediana nacional %.2f | mediana dos multimodais %.2f",
                  median(des$cv_renda_setores, na.rm = TRUE), median(m12$cv_renda_setores, na.rm = TRUE)))
  print(m12[order(-percentil_cv), .(SG_UF, NM_MUNICIPIO, setores, cv = round(cv_renda_setores, 2), percentil_cv)])
  salvar(m12, "N15_multimodais_desigualdade.csv")
}

message("\nConcluído. Resultados em ", pasta_saida, "/N15_*.csv")

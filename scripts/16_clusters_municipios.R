# =============================================================================
# 16_clusters_municipios.R - Agrupamento (K-means) dos municípios
#
# Entrada: dados_municipios.json (gerado por painel/municipios/montar_painel_municipios.py) (votação, perfil TSE, Censo,
#          território e testes por município).
# Etapas:
#   1. monta as variáveis de cada município e padroniza (z-score);
#   2. COMPARA, antes de escolher, vários critérios para o número de grupos k
#      (k = 2 a 10): cotovelo (soma de quadrados dentro), silhueta média,
#      Calinski-Harabasz, Davies-Bouldin, estatística gap, dendrograma de Ward
#      e estabilidade por reamostragem (índice de Rand ajustado);
#   3. roda o K-means final com o k escolhido e descreve os grupos.
# Os testes forenses NÃO entram no agrupamento: servem para comparar os grupos.
# Saída: clusters.json (usado pela aba "Agrupamentos" do painel)
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(cluster) })
Sys.setlocale("LC_CTYPE", "C.UTF-8")   # acentos corretos no JSON (no Windows: "pt_BR.UTF-8" ou rode com encoding = "UTF-8")
set.seed(2026)
args <- commandArgs(TRUE)
arq_dados <- if (length(args) >= 1) args[1] else "dados_municipios.json"
k_finais <- if (length(args) >= 2) as.integer(strsplit(args[2], ",")[[1]]) else integer(0)   # vazio = só compara os critérios
D <- fromJSON(arq_dados, simplifyVector = TRUE)

# ---- 1. Variáveis por município ----------------------------------------------------
cand1 <- D$cand[["1"]]; cand2 <- D$cand[["2"]]
i_l1 <- which(cand1$num == 13); i_f1 <- which(cand1$num == 22); esp1 <- cand1$esp
i_l2 <- which(cand2$num == 13); i_f2 <- which(cand2$num == 22)
# o JSON guarda [variável][município]; devolve municípios nas linhas
como_matriz <- function(x) {
  if (is.list(x)) return(do.call(cbind, lapply(x, unlist)))
  if (is.matrix(x)) return(t(x))
  matrix(x, nrow = 1)
}
linhas <- list()
for (uf in names(D$ufs)) {
  U <- D$ufs[[uf]]
  v1 <- como_matriz(U$v1); v2 <- como_matriz(U$v2)      # municípios x candidatos
  val1 <- rowSums(v1[, !esp1, drop = FALSE])
  terr <- como_matriz(U$terr); fl <- como_matriz(U$fl)
  linhas[[uf]] <- data.table(
    uf = uf, cod = U$cod, nome = U$nome, secoes = U$sec, aptos = U$apt,
    lula2 = v2[, i_l2] / (v2[, i_l2] + v2[, i_f2]),
    lula1 = v1[, i_l1] / val1, flavio1 = v1[, i_f1] / val1,
    outros1 = 1 - (v1[, i_l1] + v1[, i_f1]) / val1,
    brnul1 = rowSums(v1[, esp1, drop = FALSE]) / U$cmp1,
    abst2 = 1 - U$cmp2 / U$apt,
    fem = U$p$fem, jovens = U$p$i1624, idosos = U$p$i60, superior = U$p$sup,
    esc_baixa = U$p$baixa, biometria = U$p$bio,
    renda = U$c$renda, preta_parda = U$c$pp, indigena_censo = U$c$ind, analf = U$c$analf, esgoto = U$c$esg,
    rural = terr[, 2] / U$sec, favela = terr[, 3] / U$sec, quilombola = terr[, 4] / U$sec, indigena_terr = terr[, 5] / U$sec,
    sinalizadas = U$qual, fl = I(lapply(seq_len(nrow(fl)), function(i) fl[i, ])))
}
M <- rbindlist(linhas)
message("Municípios: ", nrow(M))

vars <- c("lula2", "lula1", "flavio1", "outros1", "brnul1", "abst2",
          "fem", "jovens", "idosos", "superior", "esc_baixa", "biometria",
          "renda", "preta_parda", "indigena_censo", "analf", "esgoto",
          "rural", "favela", "quilombola", "indigena_terr", "aptos")
blocos <- c(rep("Votação", 6), rep("Perfil do eleitorado (TSE)", 6), rep("Censo 2022", 5), rep("Território", 4), "Porte")
X <- as.data.frame(M[, ..vars])
# Censo ausente (locais sem ligação a setor): mediana da UF
for (v in c("renda", "preta_parda", "indigena_censo", "analf", "esgoto")) {
  falta <- is.na(X[[v]]); if (!any(falta)) next
  med <- M[, .(m = median(get(v), na.rm = TRUE)), by = uf]
  X[[v]][falta] <- med$m[match(M$uf[falta], med$uf)]
  message(sprintf("  %s: %d municípios sem Censo, preenchidos com a mediana da UF", v, sum(falta)))
}
# transformações para reduzir a assimetria: log na renda e no porte; raiz nas
# proporções muito concentradas em poucos municípios
X$renda <- log(X$renda); X$aptos <- log(X$aptos)
for (v in c("indigena_censo", "favela", "quilombola", "indigena_terr")) X[[v]] <- sqrt(X[[v]])
Z <- scale(as.matrix(X))
stopifnot(!anyNA(Z))

# ---- 2. Critérios para o número de grupos -------------------------------------------
# (demora ~7 min; o resultado fica guardado em criterios_k.rds para as rodadas seguintes)
dZ <- dist(Z)
hc <- hclust(dZ, method = "ward.D2")
ari <- function(a, b) { tb <- table(a, b); n <- sum(tb); s <- function(x) sum(choose(x, 2))
  e <- s(rowSums(tb)) * s(colSums(tb)) / choose(n, 2); (s(tb) - e) / ((s(rowSums(tb)) + s(colSums(tb))) / 2 - e) }
if (file.exists("criterios_k.rds")) { cache <- readRDS("criterios_k.rds"); crit <- cache$crit; votos <- cache$votos; gap_tab <- cache$gap_tab; alt <- rev(hc$height)
} else {
ks <- 2:10
km <- lapply(ks, function(k) kmeans(Z, centers = k, nstart = 50, iter.max = 200))
names(km) <- ks
wss <- sapply(km, function(f) f$tot.withinss)
tss <- km[[1]]$totss
sil <- sapply(km, function(f) mean(silhouette(f$cluster, dZ)[, 3]))
ch  <- sapply(seq_along(ks), function(j) { k <- ks[j]; n <- nrow(Z); (km[[j]]$betweenss / (k - 1)) / (km[[j]]$tot.withinss / (n - k)) })
db  <- sapply(km, function(f) {
  C <- f$centers; k <- nrow(C)
  s <- sapply(seq_len(k), function(i) mean(sqrt(rowSums((Z[f$cluster == i, , drop = FALSE] - matrix(C[i, ], sum(f$cluster == i), ncol(Z), byrow = TRUE))^2))))
  dc <- as.matrix(dist(C))
  mean(sapply(seq_len(k), function(i) max((s[i] + s[-i]) / dc[i, -i])))
})
message("Estatística gap (B = 50)...")
gp <- clusGap(Z, FUN = function(x, k) kmeans(x, k, nstart = 20, iter.max = 200), K.max = max(ks), B = 50, verbose = FALSE)
gap_tab <- as.data.table(gp$Tab)[, k := 1:max(ks)]
k_gap <- maxSE(gp$Tab[, "gap"], gp$Tab[, "SE.sim"], method = "Tibs2001SEmax")
k_gap_first <- maxSE(gp$Tab[, "gap"], gp$Tab[, "SE.sim"], method = "firstSEmax")

alt <- rev(hc$height)                   # alturas das últimas fusões (k = 2, 3, ...)
salto <- alt[ks - 1] - alt[ks]          # quanto a altura cai ao passar de k-1... (fusão que cria k grupos)
concord_ward <- sapply(ks, function(k) {   # concordância K-means x Ward (índice de Rand ajustado)
  a <- km[[as.character(k)]]$cluster; b <- cutree(hc, k); tb <- table(a, b)
  n <- sum(tb); s <- function(x) sum(choose(x, 2))
  e <- s(rowSums(tb)) * s(colSums(tb)) / choose(n, 2); (s(tb) - e) / ((s(rowSums(tb)) + s(colSums(tb))) / 2 - e)
})
message("Estabilidade por reamostragem (30 subamostras de 80%)...")
estab <- sapply(ks, function(k) {
  ref <- km[[as.character(k)]]
  mean(replicate(30, {
    i <- sample(nrow(Z), round(0.8 * nrow(Z)))
    f <- kmeans(Z[i, ], centers = k, nstart = 10, iter.max = 200)
    # atribui todos os municípios ao centro mais próximo da subamostra e compara
    d2 <- sapply(seq_len(k), function(j) rowSums((Z - matrix(f$centers[j, ], nrow(Z), ncol(Z), byrow = TRUE))^2))
    ari(ref$cluster, max.col(-d2))
  }))
})
# cotovelo: k em que a curva de WSS mais se afasta da reta entre o primeiro e o último ponto
w_all <- c(tss, wss); kk <- 1:max(ks)
dist_reta <- abs((w_all[length(kk)] - w_all[1]) * kk - (max(kk) - 1) * w_all + max(kk) * w_all[1] - w_all[length(kk)]) /
  sqrt((w_all[length(kk)] - w_all[1])^2 + (max(kk) - 1)^2)
k_cotovelo <- kk[which.max(dist_reta)]

crit <- data.table(k = ks, wss = wss, pct_explicado = 1 - wss / tss, silhueta = sil, calinski = ch, davies = db,
                   gap = gp$Tab[ks, "gap"], gap_se = gp$Tab[ks, "SE.sim"], ward_altura = alt[ks - 1], ward_salto = salto,
                   ari_ward = concord_ward, estabilidade = estab,
                   menor_grupo = sapply(km, function(f) min(f$size)))
print(crit[, lapply(.SD, function(x) round(x, 3))])
votos <- c(cotovelo = k_cotovelo, silhueta = ks[which.max(sil)], calinski = ks[which.max(ch)], davies = ks[which.min(db)],
           gap_tibs = k_gap, gap_primeiro = k_gap_first, ward_salto = ks[which.max(salto)], estabilidade = ks[which.max(estab)])
print(votos)
saveRDS(list(crit = crit, votos = votos, gap_tab = gap_tab), "criterios_k.rds")
}

# topo do dendrograma de Ward: as 29 últimas fusões, com folhas = 30 subgrupos
m_top <- tail(seq_len(nrow(hc$merge)), 29)
id_sub <- cutree(hc, 30)
# converte o "merge" do hclust para índices relativos ao topo: folhas = subgrupos 1..30
mapa <- integer(0); topo <- matrix(0L, 29, 2); prox <- 0L
folha_de <- function(x) {      # x < 0: observação; x > 0: fusão anterior ao topo -> subgrupo
  if (x < 0) return(-id_sub[-x])
  if (x %in% m_top) return(match(x, m_top))
  # fusão abaixo do topo: pega qualquer observação dela
  y <- x; while (y > 0) y <- hc$merge[y, 1]; -id_sub[-y]
}
for (j in seq_along(m_top)) topo[j, ] <- sapply(hc$merge[m_top[j], ], folha_de)
saida <- list(vars = vars, blocos = blocos, n = nrow(Z),
              criterios = crit, gap1 = gap_tab[k == 1], indicacao = as.list(votos),
              dendro = list(alturas = head(alt, 40), topo = topo, topo_altura = hc$height[m_top],
                            tam_sub = as.vector(table(id_sub))))

# ---- 3. K-means final ----------------------------------------------------------
saida$finais <- list()
for (k_escolhido in k_finais) {
  set.seed(2026)
  f <- kmeans(Z, centers = k_escolhido, nstart = 200, iter.max = 500)
  # ordena os grupos pelo % de Lula no 2º turno (grupo 1 = mais Flávio)
  ordem <- order(tapply(M$lula2, f$cluster, median)); g <- match(f$cluster, ordem)
  M[, grupo := g]
  M[, fl_cp := fl]
  silk <- silhouette(g, dZ)
  M[, silh := silk[, 3]]
  pc <- prcomp(Z)
  M[, `:=`(pc1 = pc$x[, 1], pc2 = pc$x[, 2])]
  cent_z <- t(sapply(seq_len(k_escolhido), function(j) colMeans(Z[g == j, , drop = FALSE])))
  # perfil na escala original (medianas) e médias ponderadas de testes
  perf <- M[, c(list(municipios = .N, secoes = sum(secoes), aptos = sum(aptos), sinalizadas = sum(sinalizadas),
                     silh = mean(silh)),
               lapply(.SD, median, na.rm = TRUE)), by = grupo, .SDcols = setdiff(vars, "aptos")][order(grupo)]
  perf[, aptos_mediana := M[, median(aptos), by = grupo][order(grupo), V1]]
  fl_g <- t(sapply(seq_len(k_escolhido), function(j) Reduce(`+`, M$fl[M$grupo == j])))
  # concordância com Ward no mesmo k
  ari_w <- ari(g, cutree(hc, k_escolhido))
  # dendrograma: os 30 subgrupos do topo da árvore de Ward e o grupo K-means dominante em cada um
  cortes <- cutree(hc, 30)
  sub <- data.table(folha = cortes, grupo = g)[, .(n = .N, grupo_km = as.integer(names(which.max(table(grupo))))), by = folha]
  saida$finais[[as.character(k_escolhido)]] <- list(k = k_escolhido, centros_z = cent_z, perfil = perf, testes_grupo = fl_g, ari_ward = ari_w,
                      pca_var = summary(pc)$importance[2, 1:2], pca_rot = pc$rotation[, 1:2],
                      mun = M[, .(uf, cod, grupo, silh = round(silh, 3), pc1 = round(pc1, 3), pc2 = round(pc2, 3))])
  saida$finais[[as.character(k_escolhido)]]$dendro_folhas <- sub[order(folha)]
  message(sprintf("K-means final: k = %d | %% da variância entre grupos = %.1f%% | silhueta = %.3f | ARI com Ward = %.2f",
                  k_escolhido, 100 * f$betweenss / f$totss, mean(silk[, 3]), ari_w))
  print(perf[, .(grupo, municipios, secoes, sinalizadas, pct_sin = round(100 * sinalizadas / secoes, 2), silh = round(silh, 2),
                 lula2 = round(lula2, 3), renda = round(renda), esc_baixa = round(esc_baixa, 3), rural = round(rural, 2), indigena_terr = round(indigena_terr, 3))])
}
write_json(saida, "clusters.json", auto_unbox = TRUE, digits = 6, na = "null")
message("Gravado clusters.json")

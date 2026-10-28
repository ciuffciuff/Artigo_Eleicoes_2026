# gerar_figuras_t8.R - Figuras em PDF do T8 para o artigo (artigo/Figures/)
#   fig_t8_deciles.pdf : mudança não explicada (ΔP) por decil de ΔC, com IC 95%
#   fig_t8_uf.pdf      : correlação de Spearman ΔC x ΔP por UF (linha = Brasil)
# Entrada: resultados/T8_decis.csv e resultados/T8_correlacao.csv
# Uso (na raiz do repositório): Rscript scripts/gerar_figuras_t8.R
suppressPackageStartupMessages(library(data.table))
ent <- if (dir.exists("resultados")) "resultados" else "../resultados"
sai <- file.path(dirname(normalizePath(ent)), "artigo", "Figures")
dir.create(sai, recursive = TRUE, showWarnings = FALSE)
azul <- "#2a78d6"; laranja <- "#eb6834"; tinta <- "#0b0b0b"; tinta2 <- "#52514e"; grade <- "#e6e5e1"
dispositivo <- function(f, w, h) {
  if (capabilities("cairo")) cairo_pdf(f, width = w, height = h, family = "serif")
  else pdf(f, width = w, height = h, family = "Times")
}

# ---- Figura 1: decis -------------------------------------------------------
d <- fread(file.path(ent, "T8_decis.csv"), encoding = "UTF-8")
setorder(d, decil)
dispositivo(file.path(sai, "fig_t8_deciles.pdf"), 3.5, 2.6)
par(mar = c(3.1, 3.3, 0.6, 0.6), mgp = c(2, 0.5, 0), tcl = -0.25, cex = 0.8, col.axis = tinta2, fg = tinta2)
yl <- range(c(d$ic95_inf, d$ic95_sup, 0)); yl <- yl + c(-0.02, 0.02)
plot(d$var_comparec_media_pct, d$nao_explicada_media_pp, type = "n", ylim = yl, xlim = range(d$var_comparec_media_pct) + c(-0.4, 0.4),
     xlab = "", ylab = "", axes = FALSE)
abline(h = pretty(yl), col = grade, lwd = 0.6)
abline(h = 0, col = tinta2, lty = 2, lwd = 0.8)
axis(1, lwd = 0.6); axis(2, las = 1, lwd = 0.6)
mtext(expression(Delta * C ~ "vs. municipality (%, decile mean)"), side = 1, line = 1.9, cex = 0.8, col = tinta)
mtext(expression(Delta * P ~ "toward Lula (p.p.)"), side = 2, line = 2.3, cex = 0.8, col = tinta)
segments(d$var_comparec_media_pct, d$ic95_inf, d$var_comparec_media_pct, d$ic95_sup, col = azul, lwd = 1.4)
lines(d$var_comparec_media_pct, d$nao_explicada_media_pp, col = azul, lwd = 1.5)
points(d$var_comparec_media_pct, d$nao_explicada_media_pp, pch = 21, bg = azul, col = "white", cex = 1.1, lwd = 1)
text(d$var_comparec_media_pct[c(1, 10)], d$nao_explicada_media_pp[c(1, 10)],
     labels = sprintf("%+.2f", d$nao_explicada_media_pp[c(1, 10)]), pos = c(3, 2), cex = 0.75, col = tinta)
box(bty = "l", lwd = 0.6)
invisible(dev.off())

# ---- Figura 2: correlação por UF ---------------------------------------------
co <- fread(file.path(ent, "T8_correlacao.csv"), encoding = "UTF-8")
br <- co[UF == "BRASIL", spearman]
u <- co[UF != "BRASIL"][order(spearman)]
cores <- ifelse(u$spearman >= 0, azul, laranja)
dispositivo(file.path(sai, "fig_t8_uf.pdf"), 3.5, 4.4)
par(mar = c(3.1, 2.6, 0.6, 0.8), mgp = c(2, 0.5, 0), tcl = -0.25, cex = 0.8, col.axis = tinta2, fg = tinta2)
xl <- c(min(u$spearman, 0) - 0.02, max(u$spearman) + 0.02)
y <- barplot(u$spearman, horiz = TRUE, names.arg = u$UF, las = 1, col = cores, border = NA, space = 0.35,
             xlim = xl, axes = FALSE, cex.names = 0.75)
abline(v = pretty(xl), col = grade, lwd = 0.6)
barplot(u$spearman, horiz = TRUE, names.arg = u$UF, las = 1, col = cores, border = NA, space = 0.35,
        xlim = xl, axes = FALSE, cex.names = 0.75, add = TRUE)
abline(v = 0, col = tinta2, lwd = 0.8)
abline(v = br, col = tinta, lty = 2, lwd = 0.9)
text(br, max(y) + 0.9, sprintf("Brazil %.3f", br), pos = 4, cex = 0.7, col = tinta, xpd = TRUE)
axis(1, lwd = 0.6)
mtext(expression("Spearman " * rho * " (" * Delta * C * ", " * Delta * P * ")"), side = 1, line = 1.9, cex = 0.8, col = tinta)
legend("bottomright", c("toward Lula", "toward F. Bolsonaro"), fill = c(azul, laranja), border = NA, bty = "n", cex = 0.7,
       text.col = tinta)
invisible(dev.off())
# pdflatex aceita PDF até a versão 1.5; o cairo grava 1.7. Se o qpdf existir, ajusta.
if (nzchar(Sys.which("qpdf"))) for (f in file.path(sai, c("fig_t8_deciles.pdf", "fig_t8_uf.pdf"))) {
  tmp <- paste0(f, ".tmp")
  if (system2("qpdf", c("--force-version=1.5", "--object-streams=disable", shQuote(f), shQuote(tmp))) == 0) file.rename(tmp, f)
}
message("Figuras gravadas em ", sai)

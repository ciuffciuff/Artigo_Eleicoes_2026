"""Figura N14 do artigo (rótulos em inglês). Rodar dentro de scripts/: python3 gerar_figura_n14.py"""
import pandas as pd, numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

R = "../resultados/"
OUT = "../artigo/Figures/"
C1, C2, C3 = "#2a78d6", "#eb6834", "#1baf7a"
INK, INK2, GRID = "#0b0b0b", "#52514e", "#e4e3df"
plt.rcParams.update({"font.family": "serif", "font.size": 8, "axes.edgecolor": INK2,
                     "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2,
                     "axes.spines.top": False, "axes.spines.right": False,
                     "axes.grid": True, "grid.color": GRID, "grid.linewidth": 0.5,
                     "axes.axisbelow": True, "legend.frameon": False})
d = pd.read_csv(R + "N14_decis_esquerda.csv", sep=";", encoding="utf-8-sig")
x = d.esquerda_1t_pct
fig, ax = plt.subplots(1, 2, figsize=(7.0, 2.5))
ax[0].plot(x, d.ganho_obs_pp, "o-", color=C1, ms=4, lw=1.2, label="Observed gain")
ax[0].plot(x, d.ganho_prev_pp, "s--", color=C2, ms=3.5, lw=1.0, mfc="white",
           label="Predicted from 1st round")
ax[0].plot(x, d.reserva_esq_pp, "x:", color=INK2, ms=4, lw=1.0, label="Other left-wing candidates")
ax[0].axhline(0, color=INK2, lw=0.6)
ax[0].set_xlabel("Left-wing vote, 1st round (% of valid, decile mean)")
ax[0].set_ylabel("Lula's gain, 1st round to runoff (p.p.)")
ax[0].set_title("(a) Gain by decile of left-wing strength", fontsize=8, loc="left")
ax[0].legend(loc="center", bbox_to_anchor=(0.5, 0.33), fontsize=6.5)
ax[1].errorbar(x, d.residuo_pp, yerr=[d.residuo_pp - d.ic95_inf, d.ic95_sup - d.residuo_pp],
               fmt="o", ms=4, color=C1, ecolor=INK2, lw=1, capsize=2)
ax[1].axhline(0, color=INK2, lw=0.8, ls="--")
ax[1].set_xlabel("Left-wing vote, 1st round (% of valid, decile mean)")
ax[1].set_ylabel("Observed $-$ predicted (p.p.)")
ax[1].set_title("(b) Residual centred on the municipality", fontsize=8, loc="left")
fig.tight_layout(); fig.savefig(OUT + "fig_n14_deciles.pdf"); plt.close(fig)
print("ok")

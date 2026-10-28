"""Figuras N6-N11 do artigo (rótulos em inglês). Rodar dentro de scripts/: python3 gerar_figuras_n6_n11.py"""
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
rd = lambda f: pd.read_csv(R + f, sep=";", encoding="utf-8-sig")

# ---- N6 and N7: coefficients with 95% CI ---------------------------------------
n6 = rd("N6_modelo_urna_efeito_fixo.csv")
n7 = rd("N7_perfil_coeficientes.csv")
fig, ax = plt.subplots(1, 2, figsize=(7.0, 2.5), gridspec_kw={"width_ratios": [1, 1.25]})
lab6 = {"lula_logit": "Lula share (logit)", "comparec_pct": "Turnout (p.p.)",
        "brancos_nulos_pct": "Blank+null (p.p.)"}
d = n6[n6.desfecho == "lula_logit"].copy()
d["m"] = d.termo.str.replace("modelo_", "")
for k, (t, col, off) in enumerate([(1, C1, -0.12), (2, C2, 0.12)]):
    x = d[d.turno == t]
    y = np.arange(len(x)) + off
    ax[0].errorbar(x.coef, y, xerr=1.96 * x.ep, fmt="o", ms=4, color=col, lw=1.2,
                   capsize=0, label="1st round" if t == 1 else "Runoff")
ax[0].set_yticks(range(3)); ax[0].set_yticklabels(["UE2013", "UE2015", "UE2022"])
ax[0].axvline(0, color=INK2, lw=0.8, ls="--")
ax[0].set_xlabel("Effect on Lula's logit share vs UE2020")
ax[0].set_title("(a) N6: machine model, polling-place FE", fontsize=8, loc="left")
ax[0].legend(loc="lower right", fontsize=7)
lab7 = {"p_fem": "Women", "p_16_24": "Aged 16-24", "p_45_59": "Aged 45-59",
        "p_60m": "Aged 60+", "p_esc_baixa": "Low schooling", "p_esc_sup": "Higher education",
        "p_bio": "Biometrics"}
d7 = n7[n7.alvo.str.contains("2º turno")].copy()
d7["l"] = d7.termo.map(lab7)
d7 = d7.sort_values("coef")
y = np.arange(len(d7))
cols = [C1 if c > 0 else C2 for c in d7.coef]
ax[1].errorbar(d7.coef, y, xerr=1.96 * d7.ep, fmt="none", ecolor=INK2, lw=1)
ax[1].scatter(d7.coef, y, s=18, c=cols, zorder=3, edgecolors="white", linewidths=0.6)
ax[1].set_yticks(y); ax[1].set_yticklabels(d7.l)
ax[1].axvline(0, color=INK2, lw=0.8, ls="--")
ax[1].set_xlabel("Effect of share of electorate on Lula's runoff logit")
ax[1].set_title("(b) N7: electorate profile, municipality FE", fontsize=8, loc="left")
fig.tight_layout(); fig.savefig(OUT + "fig_n6_n7_coef.pdf"); plt.close(fig)

# ---- N8: mean change by state --------------------------------------------------
n8 = rd("N8_painel_resumo.csv")
br = n8[n8.SG_UF == "BRASIL"].iloc[0]
s = n8[n8.SG_UF != "BRASIL"].sort_values("mudanca_media_pp")
fig, ax = plt.subplots(1, 2, figsize=(7.0, 3.8), sharey=True,
                       gridspec_kw={"width_ratios": [1.4, 1]})
cols = [C1 if v > 0 else C2 for v in s.mudanca_media_pp]
ax[0].barh(s.SG_UF, s.mudanca_media_pp, color=cols, height=0.7)
ax[0].axvline(br.mudanca_media_pp, color=INK, lw=0.8, ls="--")
ax[0].text(br.mudanca_media_pp + 0.15, 12.6, f"Brazil {br.mudanca_media_pp:+.2f}", fontsize=7, color=INK)
ax[0].set_xlabel("Mean change in Lula's share, 2022→2026 (p.p.)")
ax[0].set_title("(a) Mean change by state (ZZ = abroad)", fontsize=8, loc="left")
ax[1].barh(s.SG_UF, 1000 * s.sinalizadas / s.secoes, color=C3, height=0.7)
ax[1].set_xlabel("Sections with |z|>6 per 1,000")
ax[1].set_title("(b) Extreme swings", fontsize=8, loc="left")
ax[0].tick_params(axis="y", labelsize=6.5)
ax[0].set_ylim(-0.7, len(s) - 0.3)
fig.tight_layout(); fig.savefig(OUT + "fig_n8_uf.pdf"); plt.close(fig)

# ---- N9: observed - expected at each 5% focus -----------------------------------
n9 = rd("N9_rkd_por_foco.csv")
lab9 = {"Lula (% válidos)": "Lula", "Flávio (% válidos)": "F. Bolsonaro",
        "Comparecimento (% aptos)": "Turnout"}
fig, ax = plt.subplots(1, 2, figsize=(7.0, 2.4), sharey=True)
for i, t in enumerate([1, 2]):
    x = n9[n9.turno == t]
    for j, (sname, col) in enumerate([("Lula (% válidos)", C1), ("Flávio (% válidos)", C2),
                                      ("Comparecimento (% aptos)", C3)]):
        z = x[x.serie == sname]
        ax[i].bar(z.foco_pct + (j - 1) * 1.4, z.observado - z.esperado, width=1.3,
                  color=col, label=lab9[sname])
    ax[i].axhline(0, color=INK2, lw=0.8)
    ax[i].set_xticks(range(5, 100, 10))
    ax[i].set_xlabel("Round value (% , multiples of 5)")
    ax[i].set_title(f"({'ab'[i]}) {'First round' if t == 1 else 'Runoff'}", fontsize=8, loc="left")
ax[0].set_ylabel("Observed − expected sections")
ax[1].legend(fontsize=7, loc="upper right")
fig.tight_layout(); fig.savefig(OUT + "fig_n9_foci.pdf"); plt.close(fig)

# ---- N10: global Moran's I by state ---------------------------------------------
n10 = rd("N10_moran_global_por_uf.csv")
lab10 = {"z_turno": "Cross-round residual (T6)", "z_painel": "Panel residual (N8)",
         "z_perfil": "Profile residual (N7)"}
order = n10[n10.variavel == "z_perfil"].sort_values("moran_I_global").SG_UF.tolist()
pos = {u: k for k, u in enumerate(order)}
fig, ax = plt.subplots(figsize=(3.4, 3.6))
for v, col, mk in [("z_turno", C1, "o"), ("z_painel", C2, "s"), ("z_perfil", C3, "D")]:
    z = n10[n10.variavel == v]
    ax.scatter(z.moran_I_global, [pos[u] for u in z.SG_UF], s=14, c=col, marker=mk,
               label=lab10[v], edgecolors="white", linewidths=0.5, zorder=3)
ax.set_yticks(range(len(order))); ax.set_yticklabels(order, fontsize=6.5)
ax.axvline(0, color=INK2, lw=0.8, ls="--")
ax.set_xlabel("Global Moran's I (8 nearest polling places)")
ax.legend(fontsize=6.5, loc="lower right")
fig.tight_layout(); fig.savefig(OUT + "fig_n10_moran.pdf"); plt.close(fig)

# ---- N11: feature importance ----------------------------------------------------
n11 = rd("N11_ml_importancia.csv").sort_values("importancia")
lab11 = {"d_s_turno": "Change in share vs municipality", "dig_lula": "Last digit, Lula",
         "d_comp_turno": "Change in turnout vs municipality", "d_s_local": "Share vs polling place",
         "d_comp_local": "Turnout vs polling place", "dig_flavio": "Last digit, F. Bolsonaro",
         "comparec": "Turnout", "s": "Lula share", "brancos_nulos": "Blank+null share",
         "aptos": "Eligible voters", "redondo": "Integer percentage"}
fig, ax = plt.subplots(figsize=(3.4, 2.6))
cols = [C2 if v.startswith("dig") else C1 for v in n11.variavel]
ax.barh([lab11.get(v, v) for v in n11.variavel], n11.importancia / n11.importancia.sum() * 100,
        color=cols, height=0.7)
ax.set_xlabel("Importance (% of total)")
ax.tick_params(axis="y", labelsize=6.5)
fig.tight_layout(); fig.savefig(OUT + "fig_n11_importance.pdf"); plt.close(fig)
print("ok")

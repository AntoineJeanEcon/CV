# -*- coding: utf-8 -*-
"""
107_collinearity_vif.py
=======================
Diagnostic de collinearite pour la specification V3 du modele TWFE.
Calcule :
  1. Correlation de Pearson entre D_v3 et T_bar_temp (echantillon complet
     et sous-echantillon deficitaire D_v3 > 0)
  2. VIF apres within-transformation (demeanage region x annee) sur les
     cinq regresseurs de V3 :
        D_v3, D_v3 x T_bar_temp, spei_mean, T_it, t_var (sigma_T)
  Le modele complet TWFE n'est PAS estime.

Sources : data/processed/panel_estimation.csv
Outputs : results/vif_collinearity.txt
"""

import pandas as pd
import numpy as np
from statsmodels.stats.outliers_influence import variance_inflation_factor
from scipy import stats
from pathlib import Path

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT = Path(".")              # remplacez par le chemin absolu de votre projet

DATA_PATH   = ROOT / "data" / "processed" / "panel_estimation.csv"
OUTPUT_PATH = ROOT / "results" / "vif_collinearity.txt"
OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)

# ── CHARGEMENT ────────────────────────────────────────────────────────────────
df = pd.read_csv(DATA_PATH)

required_cols = ["region_id", "year", "D_v3", "T_bar_temp",
                 "spei_mean", "T_it", "t_var", "growth"]
missing = [c for c in required_cols if c not in df.columns]
if missing:
    raise ValueError(f"Colonnes manquantes dans le panel : {missing}")

# Renommage pour coherence avec la notation du memoire
df = df.rename(columns={"T_bar_temp": "T_bar", "t_var": "sigma_T"})

vars_model = ["D_v3", "T_bar", "spei_mean", "T_it", "sigma_T", "growth"]
df_clean = df[["region_id", "year"] + vars_model].dropna()
N_total  = len(df_clean)
N_deficit = (df_clean["D_v3"] > 0).sum()

print(f"Observations totales (apres dropna) : {N_total}")
print(f"Observations avec D_v3 > 0          : {N_deficit}")

# ── 1. CORRELATION DE PEARSON D_v3 / T_bar ───────────────────────────────────
r_full, p_full = stats.pearsonr(df_clean["D_v3"], df_clean["T_bar"])

df_def = df_clean[df_clean["D_v3"] > 0]
r_def, p_def = stats.pearsonr(df_def["D_v3"], df_def["T_bar"])

# ── 2. WITHIN-TRANSFORMATION (demeanage region + annee) ──────────────────────
# Demeanage sequentiel : soustrait la moyenne region puis la moyenne annee
# (approximation des effets fixes a deux dimensions, standard en TWFE)

df_w = df_clean.copy()

for col in ["D_v3", "T_bar", "spei_mean", "T_it", "sigma_T", "growth"]:
    df_w[col] = df_w[col] - df_w.groupby("region_id")[col].transform("mean")

for col in ["D_v3", "T_bar", "spei_mean", "T_it", "sigma_T", "growth"]:
    df_w[col] = df_w[col] - df_w.groupby("year")[col].transform("mean")

# Construction de l'interaction apres within-transformation
# Note : T_bar etant time-invariant, sa within-transformation par region → 0
# (quasi-nul a l'arrondi numerique). L'interaction D_v3_w x T_bar_w est donc
# quasi-identique a D_v3_w. Les VIF de l'interaction et de D_v3 peuvent
# etre artificiellement eleves — voir notes methodologiques ci-dessous.
df_w["D_v3_x_Tbar"] = df_w["D_v3"] * df_w["T_bar"]

regressors = ["D_v3", "D_v3_x_Tbar", "spei_mean", "T_it", "sigma_T"]
X = df_w[regressors].copy()
X = X.replace([np.inf, -np.inf], np.nan).dropna()

X_const      = np.column_stack([np.ones(len(X)), X.values])
col_names_vif = ["const"] + regressors

vif_results = []
for i, name in enumerate(col_names_vif):
    vif_val = variance_inflation_factor(X_const, i)
    vif_results.append((name, vif_val))

# ── 3. AFFICHAGE ET SAUVEGARDE ────────────────────────────────────────────────
lines = []
lines.append("=" * 65)
lines.append("DIAGNOSTIC DE COLLINEARITE -- SPECIFICATION V3")
lines.append("=" * 65)
lines.append(f"\nEchantillon : {N_total} observations | {df_clean['region_id'].nunique()} regions")
lines.append(f"Observations deficitaires (D_v3 > 0) : {N_deficit} ({100*N_deficit/N_total:.1f}%)")

lines.append("\n" + "-" * 65)
lines.append("1. CORRELATION DE PEARSON : D_v3 vs T_bar")
lines.append("-" * 65)
lines.append(f"  Echantillon complet   : r = {r_full:.4f}  (p = {p_full:.4e})")
lines.append(f"  D_v3 > 0 uniquement   : r = {r_def:.4f}  (p = {p_def:.4e})")

lines.append("\n" + "-" * 65)
lines.append("2. VIF APRES WITHIN-TRANSFORMATION (demeanage region + annee)")
lines.append("-" * 65)
lines.append(f"  {'Variable':<25}  {'VIF':>8}")
lines.append(f"  {'-'*25}  {'-'*8}")
for name, vif_val in vif_results:
    flag = "  *** PROBLEMATIQUE" if vif_val > 10 else ("  * Modere" if vif_val > 5 else "")
    lines.append(f"  {name:<25}  {vif_val:>8.3f}{flag}")

lines.append("\n" + "-" * 65)
lines.append("NOTES METHODOLOGIQUES")
lines.append("-" * 65)
lines.append(
    "  (a) T_bar est time-invariant : apres within-transformation\n"
    "      par region, sa variance residuelle est quasi-nulle (~0).\n"
    "      L'interaction D_v3 x T_bar_w dans le VIF porte donc sur\n"
    "      une variable proche de zero -- les VIF de cette interaction\n"
    "      et de D_v3 sont susceptibles d'etre artificiellement eleves.\n"
    "      Ce phenomene est distinct d'une vraie collinearite entre\n"
    "      les regresseurs within-transformed.\n\n"
    "  (b) La correlation de niveau (avant within) entre D_v3 et T_bar\n"
    "      est plus informative pour repondre a l'objection du referee :\n"
    "      elle mesure si les regions chaudes (T_bar eleve) connaissent\n"
    "      systematiquement des deficits SPEI plus intenses.\n\n"
    "  (c) Seuils conventionnels VIF : > 5 modere, > 10 severe.\n"
    "      Ces seuils s'appliquent aux variables within-transformed\n"
    "      effectives -- voir note (a) pour l'interaction."
)
lines.append("=" * 65)

output_text = "\n".join(lines)
print("\n" + output_text)

OUTPUT_PATH.write_text(output_text, encoding="utf-8")
print(f"\nResultats sauvegardes : {OUTPUT_PATH}")

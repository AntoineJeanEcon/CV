# -*- coding: utf-8 -*-
"""
101_baseline_regression.py
==========================
Estimation du modèle de base (équation 4.1) — trois spécifications V1/V2/V3.
Panel TWFE : effets fixes région + année, erreurs clusterisées par région.

Spécification principale (V3) :
  growth_it = β1 D_it + β2 (D_it × T̄_i) + β3 S_plus_it + β4 T_it + β5 t_var_it
              + μ_i + λ_t + ε_it

  D_it     = -min(SPEI_min_it, 0)  (choc sécheresse, positif)
  S_plus   = max(SPEI_mean_it, 0)  (excédent hydrique)
  T_it     = température moyenne ERA5
  t_var_it = trend linéaire régional
  T̄_i      = température moyenne long terme de la région (modérateur)

Sources
-------
data/processed/panel_estimation.csv

Sortie
------
results/table1_baseline.csv
results/model_v3_params.pkl
"""

from pathlib import Path
import pickle
import warnings
import numpy as np
import pandas as pd
from linearmodels.panel import PanelOLS

warnings.filterwarnings("ignore")

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT      = Path(".")         # remplacez par le chemin absolu de votre projet
DATA_FILE = ROOT / "data" / "processed" / "panel_estimation.csv"
OUT_DIR   = ROOT / "results"
OUT_DIR.mkdir(parents=True, exist_ok=True)

CSV_OUT = OUT_DIR / "table1_baseline.csv"
PKL_OUT = OUT_DIR / "model_v3_params.pkl"

# ── 1. CHARGEMENT ─────────────────────────────────────────────────────────────
print("Chargement des données...")
df = pd.read_csv(DATA_FILE)

print(f"  Observations brutes : {len(df):,}")
print(f"  Régions             : {df['region_id'].nunique():,}")
print(f"  Années              : {df['year'].nunique()} ({df['year'].min()}–{df['year'].max()})")

for col in ["D_v1", "D_v2", "D_v3", "spei_mean", "S_plus", "T_it", "t_var", "T_bar_temp", "growth"]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

# ── 2. SPÉCIFICATIONS ─────────────────────────────────────────────────────────
CONTROLS = ["S_plus", "T_it", "t_var"]

SPECS = [
    {"label": "V1", "d_col": "D_v1"},
    {"label": "V2", "d_col": "D_v2"},
    {"label": "V3", "d_col": "D_v3"},
]

ROW_LABELS = ["D_it", "D_it x T_bar", "S_plus", "T_it", "t_var"]

def stars(pval):
    if pval < 0.01:  return "***"
    if pval < 0.05:  return "**"
    if pval < 0.10:  return "*"
    return ""

# ── 3. ESTIMATION ─────────────────────────────────────────────────────────────
results   = {}
summaries = {}

for spec in SPECS:
    lbl   = spec["label"]
    d_col = spec["d_col"]
    inter = f"{d_col}_x_Tbar"

    print(f"\nEstimation {lbl} (D_it = {d_col})...")

    dw      = df[["region_id", "year", "growth", d_col, "T_bar_temp"] + CONTROLS].copy()
    dw[inter] = dw[d_col] * dw["T_bar_temp"]

    model_vars = ["growth", d_col, inter] + CONTROLS
    dw = dw.dropna(subset=model_vars).set_index(["region_id", "year"])

    exog  = dw[[d_col, inter] + CONTROLS]
    endog = dw["growth"]

    model = PanelOLS(dependent=endog, exog=exog,
                     entity_effects=True, time_effects=True)
    fit   = model.fit(cov_type="clustered", cluster_entity=True)

    results[lbl]   = fit
    summaries[lbl] = {
        "params": fit.params,
        "se":     fit.std_errors,
        "pvals":  fit.pvalues,
        "N":      int(fit.nobs),
        "r2_w":   fit.rsquared_within,
        "d_col":  d_col,
        "inter":  inter,
    }

    print(f"  N = {fit.nobs:,}  |  R² within = {fit.rsquared_within:.4f}")
    print(f"  β(D_it)      = {fit.params[d_col]:.5f}  (SE={fit.std_errors[d_col]:.5f}){stars(fit.pvalues[d_col])}")
    print(f"  β(D_it×Tbar) = {fit.params[inter]:.6f}  (SE={fit.std_errors[inter]:.6f}){stars(fit.pvalues[inter])}")

# ── 4. SAUVEGARDE MODÈLE V3 ───────────────────────────────────────────────────
print(f"\nSauvegarde du modèle V3 → {PKL_OUT}")
with open(PKL_OUT, "wb") as f:
    pickle.dump(results["V3"], f)

# ── 5. TABLEAU ────────────────────────────────────────────────────────────────
VAR_MAP = {
    lbl: {
        "D_it":        summaries[lbl]["d_col"],
        "D_it x T_bar": summaries[lbl]["inter"],
        "S_plus":      "S_plus",
        "T_it":        "T_it",
        "t_var":       "t_var",
    }
    for lbl in ["V1", "V2", "V3"]
}

header = f"{'Variable':<18}{'V1 coef':>12}{'V1 SE':>10}{'V2 coef':>12}{'V2 SE':>10}{'V3 coef':>12}{'V3 SE':>10}"
sep    = "-" * len(header)
print(f"\n{'='*len(header)}")
print("TABLEAU 1 — Estimation baseline (TWFE, SE clusterisées par région)")
print(f"{'='*len(header)}")
print(header); print(sep)

rows_for_csv = []

for row_lbl in ROW_LABELS:
    row_str = f"{row_lbl:<18}"
    csv_row = {"variable": row_lbl}
    for spec_lbl in ["V1", "V2", "V3"]:
        col_name = VAR_MAP[spec_lbl][row_lbl]
        s        = summaries[spec_lbl]
        coef     = s["params"][col_name]
        se       = s["se"][col_name]
        pval     = s["pvals"][col_name]
        row_str += f"{f'{coef:.5f}{stars(pval)}':>12}{se:>10.5f}"
        pfx = spec_lbl.lower()
        csv_row[f"{pfx}_coef"] = coef
        csv_row[f"{pfx}_se"]   = se
        csv_row[f"{pfx}_pval"] = pval
    print(row_str)
    rows_for_csv.append(csv_row)

print(sep)

for meta_lbl, meta_key in [("N", "N"), ("R2_within", "r2_w")]:
    row_str = f"{meta_lbl:<18}"
    csv_row = {"variable": meta_lbl}
    for spec_lbl in ["V1", "V2", "V3"]:
        val = summaries[spec_lbl][meta_key]
        pfx = spec_lbl.lower()
        row_str += f"{val:>12,}{'':>10}" if meta_lbl == "N" else f"{val:>12.4f}{'':>10}"
        csv_row[f"{pfx}_coef"] = val
        csv_row[f"{pfx}_se"]   = np.nan
        csv_row[f"{pfx}_pval"] = np.nan
    print(row_str)
    rows_for_csv.append(csv_row)

print(f"{'='*len(header)}")
print("Signif. : *** p<0.01  ** p<0.05  * p<0.10")
print(f"Effets fixes : région + année  |  Clustering : région")

col_order = ["variable",
             "v1_coef", "v1_se", "v1_pval",
             "v2_coef", "v2_se", "v2_pval",
             "v3_coef", "v3_se", "v3_pval"]
pd.DataFrame(rows_for_csv)[col_order].to_csv(CSV_OUT, index=False, float_format="%.6f")
print(f"\nCSV sauvegardé → {CSV_OUT}")
print("Script terminé.")

# -*- coding: utf-8 -*-
"""
103_conley_se.py
================
Erreurs standard de Conley (1999) — implémentation vectorisée.
Sandwich spatial construit année par année avec opérations matricielles
numpy (aucune boucle sur les paires de régions).

Le script calcule les SE de Conley pour :
  - La baseline V3 (β1 sur D_v3, β2 sur D_v3 × T̄, AME)
    aux rayons de troncature 500, 1000, 2000 km
  - Le distributed lag model (lags 0–4, Σβ)
    au rayon 1000 km

Noyau spatial : Bartlett uniforme  w(d) = max(0, 1 - d/cutoff)
Noyau temporel : Bartlett  w(l) = 1 - l/(L+1)  pour L lags

Sources
-------
data/processed/panel_estimation.csv
data/raw/gadm36_levels_shp/gadm36_1.shp   (centroïdes régionaux)

Sortie
------
results/diagnostic_conley.csv
results/dlm_cov_lags.npy   (matrice de covariance 5×5 des lags DLM)
"""

import sys
import warnings; warnings.filterwarnings("ignore")
import numpy as np
import pandas as pd
from pathlib import Path
from datetime import datetime
from linearmodels.panel import PanelOLS
from scipy.stats import norm as snorm
import geopandas as gpd

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT    = Path(".")           # remplacez par le chemin absolu de votre projet
CSV_IN  = ROOT / "data" / "processed" / "panel_estimation.csv"
SHP1    = ROOT / "data" / "raw" / "gadm36_levels_shp" / "gadm36_1.shp"
CSV_OUT = ROOT / "results" / "diagnostic_conley.csv"
COV_OUT = ROOT / "results" / "dlm_cov_lags.npy"

T_BAR    = 18.24   # température moyenne du panel (pour le calcul de l'AME)
Z95      = 1.96
CUTOFFS  = [500, 1000, 2000]   # rayons de troncature (km)
L_LAG    = 3                   # nombre de lags temporels Bartlett
LAG_VARS = ["D_v3", "D_v3_lag1", "D_v3_lag2", "D_v3_lag3", "D_v3_lag4"]
BETA_REF = np.array([-0.00982, 0.00960, -0.00694, -0.00115, 0.00389])  # coefficients DLM

def log(msg=""):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)

# ── 1. CENTROÏDES ─────────────────────────────────────────────────────────────
log("="*65); log("1. CENTROÏDES GADM-1 (geopandas)"); log("="*65)
gdf = gpd.read_file(SHP1)[["GID_1", "geometry"]]
gdf["lon"] = gdf.geometry.centroid.x
gdf["lat"] = gdf.geometry.centroid.y
geo_df = gdf[["GID_1", "lat", "lon"]].rename(columns={"GID_1": "region_id"}).set_index("region_id")

# ── 2. PANEL ──────────────────────────────────────────────────────────────────
log("\n"+"="*65); log("2. PRÉPARATION DU PANEL"); log("="*65)
df = pd.read_csv(CSV_IN)
for c in ["D_v3", "S_plus", "T_it", "t_var", "T_bar_temp", "growth", "spei_mean"] + LAG_VARS:
    if c in df.columns:
        df[c] = pd.to_numeric(df[c], errors="coerce")
df["D_v3"]        = df["D_v3"].clip(lower=0)
df["D_v3_x_Tbar"] = df["D_v3"] * df["T_bar_temp"]
df["S_plus"]      = np.maximum(df["spei_mean"], 0)

# ── 3. ESTIMATION OLS ─────────────────────────────────────────────────────────
log("\n"+"="*65); log("3. ESTIMATION OLS"); log("="*65)

# Baseline V3
COLS_BASE = ["region_id", "year", "D_v3", "D_v3_x_Tbar", "S_plus", "T_it", "t_var", "growth"]
df_base   = df[COLS_BASE].dropna().set_index(["region_id", "year"])
FORM_BASE = "growth ~ 1 + D_v3 + D_v3_x_Tbar + S_plus + T_it + t_var + EntityEffects + TimeEffects"
res_b     = PanelOLS.from_formula(FORM_BASE, data=df_base).fit(
                cov_type="clustered", cluster_entity=True, use_lsdv=True)
b1v = float(res_b.params["D_v3"])
b2v = float(res_b.params["D_v3_x_Tbar"])
log(f"  Baseline OK b1={b1v:+.6f} b2={b2v:+.7f}")

# DLM
COLS_D  = ["region_id", "year", "growth", "S_plus", "T_it", "t_var"] + LAG_VARS
df_d    = df[COLS_D].dropna().set_index(["region_id", "year"])
FORM_D  = "growth ~ 1 + " + " + ".join(LAG_VARS) + " + S_plus + T_it + t_var + EntityEffects + TimeEffects"
res_d   = PanelOLS.from_formula(FORM_D, data=df_d).fit(
              cov_type="clustered", cluster_entity=True, use_lsdv=True)
betas_d = np.array([float(res_d.params[v]) for v in LAG_VARS])
log(f"  DLM OK betas={betas_d.round(5)} Sigma={betas_d.sum():.5f}")

# ── FONCTIONS CONLEY ──────────────────────────────────────────────────────────
def haversine_matrix(lat, lon):
    R  = 6371.0
    φ  = np.radians(lat)[:, None];  φ2 = np.radians(lat)[None, :]
    Δφ = φ2 - φ;                    Δλ = np.radians(lon)[None, :] - np.radians(lon)[:, None]
    a  = np.sin(Δφ/2)**2 + np.cos(φ) * np.cos(φ2) * np.sin(Δλ/2)**2
    return 2 * R * np.arcsin(np.sqrt(np.clip(a, 0, 1)))

def conley_meat(X_dm, e, regions, years, lat_s, lon_s, cutoff_km, L_lag):
    K       = X_dm.shape[1]
    S       = np.zeros((K, K))
    yr_vals = np.unique(years)
    yr_set  = set(yr_vals)
    G_by_yr = {}; lat_by = {}; lon_by = {}; reg_by = {}
    for yr in yr_vals:
        m = (years == yr)
        G_by_yr[yr] = X_dm[m] * e[m, None]
        lat_by[yr]  = lat_s[m]; lon_by[yr] = lon_s[m]; reg_by[yr] = regions[m]
    for yr in yr_vals:
        G_t = G_by_yr[yr]; lat_t = lat_by[yr]; lon_t = lon_by[yr]
        if len(G_t) == 0: continue
        D_t = haversine_matrix(lat_t, lon_t)
        W_t = np.maximum(0.0, 1.0 - D_t / cutoff_km)
        S  += G_t.T @ W_t @ G_t
        for l in range(1, L_lag + 1):
            yr_l = yr - l
            if yr_l not in yr_set: continue
            w_l   = 1.0 - l / (L_lag + 1)
            G_l   = G_by_yr[yr_l]; reg_l = reg_by[yr_l]; lat_l = lat_by[yr_l]; lon_l = lon_by[yr_l]
            reg_t = reg_by[yr]
            common, it, il = np.intersect1d(reg_t, reg_l, return_indices=True)
            if len(common) == 0: continue
            G_tc = G_t[it]; G_lc = G_l[il]
            lat_tc = lat_t[it]; lon_tc = lon_t[it]
            D_cross = haversine_matrix(lat_tc, lon_tc)
            W_cross = np.maximum(0.0, 1.0 - D_cross / cutoff_km)
            S += w_l * (G_tc.T @ W_cross @ G_lc + G_lc.T @ W_cross @ G_tc)
    return S

def vcov_conley(X_dm, e, regions, years, lat_s, lon_s, cutoff_km, L_lag):
    K  = X_dm.shape[1]; N = len(e)
    S  = conley_meat(X_dm, e, regions, years, lat_s, lon_s, cutoff_km, L_lag)
    Xi = np.linalg.pinv(X_dm.T @ X_dm)
    return (N / (N - K)) * (Xi @ S @ Xi)

# ── 4. EXTRACTION RÉSIDUS + DEMEAN ────────────────────────────────────────────
def get_residuals_and_Xdm(res, reg_vars, geo_df):
    idx     = res.resids.index
    regions = np.array([i[0] for i in idx])
    years   = np.array([i[1] for i in idx])
    e       = res.resids.values.astype(float)
    lat_s   = np.array([geo_df.loc[r, "lat"] if r in geo_df.index else np.nan for r in regions])
    lon_s   = np.array([geo_df.loc[r, "lon"] if r in geo_df.index else np.nan for r in regions])
    X_dm    = res.model.exog.values2d[:len(e), :len(reg_vars)]   # colonnes des régresseurs
    # Remarque : X_dm ici correspond aux colonnes within-demeaned retournées par use_lsdv=True
    return regions, years, e, lat_s, lon_s, X_dm

# ── 5. BASELINE — SE CONLEY ───────────────────────────────────────────────────
log("\n"+"="*65); log("5. BASELINE V3 — SE CONLEY"); log("="*65)

regions_b = np.array([i[0] for i in res_b.resids.index])
years_b   = np.array([i[1] for i in res_b.resids.index])
e_b       = res_b.resids.values.astype(float)
lat_b     = np.array([geo_df.loc[r, "lat"] if r in geo_df.index else np.nan for r in regions_b])
lon_b     = np.array([geo_df.loc[r, "lon"] if r in geo_df.index else np.nan for r in regions_b])
X_b_dm    = res_b.estimated_effects.model.data.data.values if hasattr(res_b, "estimated_effects") else None

# Approche robuste : reconstruire X_dm depuis les données brutes
BASE_REGS = ["D_v3", "D_v3_x_Tbar", "S_plus", "T_it", "t_var"]
df_b_vals = df_base[BASE_REGS]

def within_demean_twoway(X_raw, regions, years):
    """Demean within two-way fixed effects (alternating projection)."""
    K = X_raw.shape[1]
    X = X_raw.copy().astype(float)
    idx = pd.MultiIndex.from_arrays([regions, years])
    for ci in range(K):
        s = pd.Series(X[:, ci], index=idx)
        mu_r = s.groupby(level=0).transform("mean").values
        mu_y = s.groupby(level=1).transform("mean").values
        X[:, ci] = X[:, ci] - mu_r - mu_y + s.mean()
    return X

log("  Demean within two-way (baseline)...")
idx_b      = res_b.resids.index
X_b_raw    = df_base[BASE_REGS].loc[idx_b].values.astype(float)
X_b_dm     = within_demean_twoway(X_b_raw, regions_b, years_b)

results_b = {}
for cutoff in CUTOFFS:
    log(f"  Cutoff {cutoff} km...")
    V_b    = vcov_conley(X_b_dm, e_b, regions_b, years_b, lat_b, lon_b, cutoff, L_LAG)
    se_b1  = np.sqrt(max(V_b[0, 0], 0))
    se_b2  = np.sqrt(max(V_b[1, 1], 0))
    t_b1   = b1v / se_b1; p_b1 = 2 * snorm.sf(abs(t_b1))
    t_b2   = b2v / se_b2; p_b2 = 2 * snorm.sf(abs(t_b2))
    # AME = β1 + β2 × T̄
    ame    = b1v + b2v * T_BAR
    V12    = V_b[0, 1]
    se_ame = np.sqrt(max(V_b[0,0] + T_BAR**2 * V_b[1,1] + 2*T_BAR*V12, 0))
    t_ame  = ame / se_ame; p_ame = 2 * snorm.sf(abs(t_ame))
    results_b[cutoff] = dict(se_b1=se_b1, p_b1=p_b1, se_b2=se_b2, p_b2=p_b2,
                              se_ame=se_ame, p_ame=p_ame)
    log(f"    SE(b1)={se_b1:.6f} p(b1)={p_b1:.4f} | SE(b2)={se_b2:.7f} p(b2)={p_b2:.4f} | SE(AME)={se_ame:.6f} p(AME)={p_ame:.4f}")

# ── 6. DLM — SE CONLEY (1000 km) ─────────────────────────────────────────────
log("\n"+"="*65); log("6. DLM — SE CONLEY (1000 km)"); log("="*65)

regions_d = np.array([i[0] for i in res_d.resids.index])
years_d   = np.array([i[1] for i in res_d.resids.index])
e_d       = res_d.resids.values.astype(float)
lat_d     = np.array([geo_df.loc[r, "lat"] if r in geo_df.index else np.nan for r in regions_d])
lon_d     = np.array([geo_df.loc[r, "lon"] if r in geo_df.index else np.nan for r in regions_d])

DLM_REGS  = LAG_VARS + ["S_plus", "T_it", "t_var"]
idx_d     = res_d.resids.index
X_d_raw   = df_d[DLM_REGS].loc[idx_d].values.astype(float)

log("  Demean within two-way (DLM)...")
X_d_dm = within_demean_twoway(X_d_raw, regions_d, years_d)

log(f"  Cutoff {1000} km (DLM)...")
V_d = vcov_conley(X_d_dm, e_d, regions_d, years_d, lat_d, lon_d, 1000, L_LAG)
V5  = V_d[:5, :5]

dlm_se_c = {}
for i, v in enumerate(LAG_VARS):
    se_i = np.sqrt(max(V5[i, i], 0))
    dlm_se_c[v] = se_i
    log(f"    {v}: beta={betas_d[i]:+.5f}  SE(reg)={float(res_d.std_errors[v]):.6f}  SE(Conley 1000)={se_i:.6f}")

e_vec = np.ones(5)
sig   = betas_d.sum()
se_s  = np.sqrt(max(float(e_vec @ V5 @ e_vec), 0))
p_s   = 2 * snorm.sf(abs(sig / se_s))
ic_lo = sig - Z95 * se_s
ic_hi = sig + Z95 * se_s
log(f"  Sigma_beta: SE={se_s:.6f}  IC95=[{ic_lo:.5f} ; {ic_hi:.5f}]  p={p_s:.4f}")

np.save(COV_OUT, V5)
log(f"  Covariance DLM Conley 5×5 sauvegardée → {COV_OUT}")

# ── 7. SAUVEGARDE CSV ─────────────────────────────────────────────────────────
rows = []
for cutoff in CUTOFFS:
    r = results_b[cutoff]
    for k, v in r.items():
        rows.append({"section": "baseline", "cutoff_km": cutoff, "stat": k, "value": v})
for k, v in [("SE_sigma", se_s), ("p_sigma", p_s), ("IC95_low", ic_lo), ("IC95_high", ic_hi)]:
    rows.append({"section": "DLM", "cutoff_km": 1000, "stat": k, "value": v})
for i, v in enumerate(LAG_VARS):
    rows.append({"section": "DLM", "cutoff_km": 1000, "stat": f"SE_{v}", "value": dlm_se_c[v]})

pd.DataFrame(rows).to_csv(CSV_OUT, index=False)
log(f"\n  CSV : {CSV_OUT}")
log("Script terminé.")

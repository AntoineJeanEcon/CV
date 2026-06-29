# -*- coding: utf-8 -*-
"""
200_attribution.py
==================
Attribution économique du forçage anthropogénique sur la croissance régionale.
Combine les deltas SPEI CMIP6 avec les coefficients économétriques estimés
sur le panel (spec DLM, lags 0–4).

Logique d'attribution (suivant Callahan & Mankin 2022) :
  1. Centroïdes des régions GADM niveau 1 extraits depuis gadm36_1.shp
  2. Agrégation du delta SPEI ensemble au niveau région (pixel le plus proche)
  3. Transformation asymétrique : ΔD_it = -min(Δ SPEI_it, 0) ≥ 0
  4. Effet attribué cumulé 5 ans : Σ β̂_k × ΔD_i,t-k
  5. Monte Carlo croisé N=1000 sur incertitude β × incertitude modèle CMIP6
     (chaque réplication tire β ~ N(BETA, diag(SE²)) et un modèle CMIP6 au hasard)

Les 10 matrices delta_D région×année sont pré-chargées en mémoire avant
la boucle Monte Carlo pour éviter les I/O répétés.

Sources
-------
data/cmip6/processed/delta_spei_ensemble.nc
data/cmip6/processed/delta_spei_{model}.nc   (pour chaque modèle)
data/processed/panel_estimation.csv
data/raw/gadm36_levels_shp/gadm36_1.shp

Sortie
------
results/attribution/attribution_regional.csv
results/attribution/attribution_global.csv
results/attribution/attribution_by_model.csv
results/attribution/attribution_log.txt
"""

import numpy as np
import pandas as pd
import xarray as xr
import geopandas as gpd
from pathlib import Path
from datetime import datetime

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT       = Path(".")        # remplacez par le chemin absolu de votre projet
DELTA_PATH = ROOT / "data/cmip6/processed/delta_spei_ensemble.nc"
PANEL_PATH = ROOT / "data/processed/panel_estimation.csv"
GADM_PATH  = ROOT / "data/raw/gadm36_levels_shp/gadm36_1.shp"
OUT_DIR    = ROOT / "results/attribution"
LOG_PATH   = OUT_DIR / "attribution_log.txt"
OUT_DIR.mkdir(parents=True, exist_ok=True)

PROC_DIR = ROOT / "data/cmip6/processed"

# Coefficients DLM estimés (β̂_k lag 0-4) — spécification (b) S_plus
# β3 non significatif (p=0.339) ; tous autres p<0.001
BETA = np.array([-0.00982,  0.00960, -0.00694, -0.00115,  0.00389])
SE   = np.array([ 0.00144,  0.00124,  0.00113,  0.00120,  0.00106])

N_MC       = 1000
YEAR_START = 1993
YEAR_END   = 2013
N_LAGS     = 4

MODELS = [
    "MIROC6", "CanESM5", "IPSL-CM6A-LR", "CNRM-CM6-1", "HadGEM3-GC31-LL",
    "MRI-ESM2-0", "BCC-CSM2-MR", "GFDL-ESM4", "ACCESS-ESM1-5", "NorESM2-LM",
]

# ── HELPERS ───────────────────────────────────────────────────────────────────
LOG_LINES = []

def log(msg=""):
    ts   = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    line = f"{ts} {msg}"
    print(line)
    LOG_LINES.append(line)

# ── MAIN ──────────────────────────────────────────────────────────────────────
log("=" * 70)
log("200_attribution.py — Attribution anthropogénique + MC croisé")
log(f"β̂ DLM lags 0-4 : {BETA}")
log(f"SE              : {SE}")
log(f"Monte Carlo N   : {N_MC}")
log(f"Fenêtre         : {YEAR_START}–{YEAR_END}")
log("=" * 70)

# ── 1. LECTURE DELTA ENSEMBLE ─────────────────────────────────────────────────
log("\n[1/6] Lecture delta SPEI ensemble")
ds_delta = xr.open_dataset(DELTA_PATH, engine="netcdf4")
da_delta = ds_delta["delta_spei"].load()
ds_delta.close()
log(f"  Shape  : {da_delta.shape} (year, lat, lon)")
log(f"  Années : {int(da_delta.year.min())}–{int(da_delta.year.max())}")
log(f"  Delta nanmean : {float(np.nanmean(da_delta.values)):.4f}")

# ── 2. PANEL + CENTROÏDES GADM ────────────────────────────────────────────────
log("\n[2/6] Lecture panel + centroïdes GADM")
panel = pd.read_csv(PANEL_PATH)
log(f"  Panel : {len(panel):,} obs | {panel['region_id'].nunique():,} régions")

log(f"  Lecture shapefile : {GADM_PATH.name}")
gdf = gpd.read_file(GADM_PATH)
gdf["centroid"] = gdf.geometry.centroid
gdf["lat"]      = gdf["centroid"].y
gdf["lon"]      = gdf["centroid"].x
centroids = gdf[["GID_1", "lat", "lon"]].rename(columns={"GID_1": "region_id"})

regions_panel = set(panel["region_id"].unique())
centroids = centroids[centroids["region_id"].isin(regions_panel)].reset_index(drop=True)
log(f"  Centroïdes disponibles : {len(centroids):,} / {len(regions_panel):,} régions")

# ── 3. AGRÉGATION DELTA ENSEMBLE → RÉGIONS GADM ──────────────────────────────
log("\n[3/6] Agrégation delta SPEI ensemble → régions GADM (pixel le plus proche)")

lat_arr    = da_delta.lat.values
lon_arr    = da_delta.lon.values
years      = da_delta.year.values
delta_vals = da_delta.values  # (year, lat, lon)

records = []
for _, row in centroids.iterrows():
    region_id = row["region_id"]
    rlat      = row["lat"]
    rlon_360  = row["lon"] + 360 if row["lon"] < 0 else row["lon"]
    i_lat     = int(np.argmin(np.abs(lat_arr - rlat)))
    i_lon     = int(np.argmin(np.abs(lon_arr - rlon_360)))
    for y_idx, year in enumerate(years):
        records.append({"region_id": region_id, "year": int(year),
                        "delta_spei": float(delta_vals[y_idx, i_lat, i_lon])})

df_delta = pd.DataFrame(records)
log(f"  {len(df_delta):,} obs région-année")
log(f"  Delta moyen (nanmean) : {df_delta['delta_spei'].mean():.4f}")

# ── 4. CHOC ASYMÉTRIQUE ΔD = -min(Δ SPEI, 0) ─────────────────────────────────
log("\n[4/6] Transformation asymétrique ΔD_it = -min(Δ SPEI, 0) — ensemble moyen")
df_delta["delta_D"] = -np.minimum(df_delta["delta_spei"], 0)
pct_pos  = (df_delta["delta_D"] > 0).mean() * 100
log(f"  Proportion ΔD > 0 (déficits anthropogéniques) : {pct_pos:.1f}%")
log(f"  ΔD moyen global : {df_delta['delta_D'].mean():.4f}")

# ── 4b. PRÉ-CHARGEMENT DES 10 MATRICES DELTA_D PAR MODÈLE ────────────────────
log("\n[4b/6] Pré-chargement des matrices delta_D par modèle CMIP6")

region_order = centroids["region_id"].tolist()
n_regions    = len(region_order)
years_ref    = sorted(df_delta["year"].unique())
n_years      = len(years_ref)
year_to_idx  = {y: i for i, y in enumerate(years_ref)}

model_delta_D = {}
models_loaded = []

for model in MODELS:
    path_m = PROC_DIR / f"delta_spei_{model}.nc"
    if not path_m.exists():
        log(f"  SKIP {model} : fichier absent")
        continue

    ds_m    = xr.open_dataset(path_m, engine="netcdf4")
    da_m    = ds_m["delta_spei"].load()
    ds_m.close()

    lat_m   = da_m.lat.values
    lon_m   = da_m.lon.values
    years_m = [int(y) for y in da_m.year.values]

    mat = np.full((n_regions, n_years), np.nan)
    for r_idx, row in centroids.iterrows():
        rlon_360 = row["lon"] + 360 if row["lon"] < 0 else row["lon"]
        i_lat    = int(np.argmin(np.abs(lat_m - row["lat"])))
        i_lon    = int(np.argmin(np.abs(lon_m - rlon_360)))
        for y_pos, year_m in enumerate(years_m):
            if year_m in year_to_idx:
                raw = da_m.values[y_pos, i_lat, i_lon]
                mat[r_idx, year_to_idx[year_m]] = -min(float(raw), 0) if not np.isnan(raw) else np.nan

    model_delta_D[model] = mat
    models_loaded.append(model)
    log(f"  Chargé {model:20s} : ΔD moyen = {float(np.nanmean(mat)):.4f}")

n_models_mc = len(models_loaded)
log(f"  {n_models_mc} modèles chargés pour le MC croisé")
if n_models_mc == 0:
    raise RuntimeError("Aucun fichier delta_spei_{model}.nc trouvé. Vérifier PROC_DIR.")

# ── 5. EFFET ATTRIBUÉ + MONTE CARLO CROISÉ ───────────────────────────────────
log(f"\n[5/6] Calcul effet attribué + Monte Carlo croisé N={N_MC}")

df_delta = df_delta.sort_values(["region_id", "year"]).reset_index(drop=True)
for k in range(1, N_LAGS + 1):
    df_delta[f"delta_D_lag{k}"] = df_delta.groupby("region_id")["delta_D"].shift(k)

lag_cols = ["delta_D"] + [f"delta_D_lag{k}" for k in range(1, N_LAGS + 1)]

df_delta["effect_instant"] = BETA[1] * df_delta["delta_D"]
df_delta["effect_cumul5"]  = sum(BETA[k] * df_delta[lag_cols[k]].fillna(0)
                                 for k in range(N_LAGS + 1))

np.random.seed(42)
beta_mc     = np.random.multivariate_normal(BETA, np.diag(SE**2), size=N_MC)
model_draws = np.random.randint(0, n_models_mc, size=N_MC)

n_obs     = len(df_delta)
effect_mc = np.full((N_MC, n_obs), np.nan)

region_ids_sorted = df_delta["region_id"].values
year_vals_sorted  = df_delta["year"].values
region_to_ridx    = {rid: i for i, rid in enumerate(region_order)}

for sim in range(N_MC):
    if sim % 100 == 0:
        log(f"  Simulation {sim}/{N_MC} ...")

    beta_sim  = beta_mc[sim]
    model_sim = models_loaded[model_draws[sim]]
    mat_sim   = model_delta_D[model_sim]

    # Construire la série delta_D pour ce modèle, par région
    dD_sim = np.full(n_obs, np.nan)
    for obs_idx in range(n_obs):
        rid  = region_ids_sorted[obs_idx]
        year = year_vals_sorted[obs_idx]
        r_i  = region_to_ridx.get(rid, None)
        y_i  = year_to_idx.get(year, None)
        if r_i is not None and y_i is not None:
            dD_sim[obs_idx] = mat_sim[r_i, y_i]

    # Effets cumulés 5 lags par région
    eff = np.zeros(n_obs)
    for k in range(N_LAGS + 1):
        if k == 0:
            dD_k = dD_sim.copy()
        else:
            dD_k = np.full(n_obs, np.nan)
            for obs_idx in range(n_obs):
                rid  = region_ids_sorted[obs_idx]
                year = year_vals_sorted[obs_idx] - k
                r_i  = region_to_ridx.get(rid, None)
                y_i  = year_to_idx.get(year, None)
                if r_i is not None and y_i is not None:
                    dD_k[obs_idx] = mat_sim[r_i, y_i]

        mask_k = ~np.isnan(dD_k)
        eff[mask_k] += beta_sim[k] * dD_k[mask_k]

    effect_mc[sim] = eff

log(f"  MC terminé.")

# ── 6. SAUVEGARDE ─────────────────────────────────────────────────────────────
log("\n[6/6] Sauvegarde des résultats")

df_delta["effect_cumul5_mc_mean"]  = np.nanmean(effect_mc, axis=0)
df_delta["effect_cumul5_mc_p5"]    = np.nanpercentile(effect_mc, 5,  axis=0)
df_delta["effect_cumul5_mc_p95"]   = np.nanpercentile(effect_mc, 95, axis=0)

region_summary = (
    df_delta.groupby("region_id")
    .agg(
        effect_instant_mean=("effect_instant", "mean"),
        effect_cumul5_mean=("effect_cumul5",   "mean"),
        effect_mc_mean=("effect_cumul5_mc_mean", "mean"),
        effect_mc_p5=("effect_cumul5_mc_p5",     "mean"),
        effect_mc_p95=("effect_cumul5_mc_p95",   "mean"),
        n_years=("effect_cumul5", "count"),
    )
    .reset_index()
)

out_reg = OUT_DIR / "attribution_regional.csv"
region_summary.to_csv(out_reg, index=False)
log(f"  → {out_reg}")

global_summary = {
    "effect_instant_mean":  df_delta["effect_instant"].mean(),
    "effect_cumul5_mean":   df_delta["effect_cumul5"].mean(),
    "effect_mc_mean":       float(np.nanmean(effect_mc)),
    "effect_mc_p5":         float(np.nanpercentile(effect_mc, 5)),
    "effect_mc_p95":        float(np.nanpercentile(effect_mc, 95)),
    "n_obs":                n_obs,
}
out_global = OUT_DIR / "attribution_global.csv"
pd.DataFrame([global_summary]).to_csv(out_global, index=False)
log(f"  → {out_global}")

for stat, val in global_summary.items():
    log(f"    {stat:<30s} : {val}")

LOG_PATH.write_text("\n".join(LOG_LINES), encoding="utf-8")
log("Script terminé.")

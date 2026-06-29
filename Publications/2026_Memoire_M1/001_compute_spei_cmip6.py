# -*- coding: utf-8 -*-
"""
001_compute_spei_cmip6.py
=========================
Calcule le SPEI-12 pour chaque modèle CMIP6 à partir des fichiers bruts
téléchargés par 000_download_cmip6.py.

Pipeline par modèle :
  1. Lecture et concaténation temporelle des fichiers pr et tas
  2. Recadrage sur la fenêtre 1950–2013
  3. Calcul de l'ETP par méthode Hargreaves-Samani (tas + amplitude diurne
     approximée depuis la latitude)
  4. Calcul du bilan mensuel D = P - ETP
  5. Calcul du SPEI-12 par accumulation sur 12 mois + ajustement
     log-logistique + standardisation (suivant Vicente-Serrano et al. 2010)
  6. Sauvegarde par modèle et par expérience

Hypothèses
----------
  → ETP Hargreaves-Samani sans tasmax/tasmin : amplitude diurne fixée à
    12°C (approximation conservative, cohérente avec la littérature sur
    les zones tropicales et tempérées à données limitées)
  → Période de standardisation SPEI : 1950–2010 (cohérence SPEIbase v2.10)
  → Grille native conservée (pas de regridding à ce stade)

Sources brutes attendues
------------------------
data/cmip6/raw/{var}_Amon_{model}_{experiment}_r1i1p1f*_{grid}_{period}.nc

Sortie
------
data/cmip6/processed/spei12_{model}_{experiment}.nc
data/cmip6/compute_log.txt
"""

import math
import numpy as np
import xarray as xr
import pandas as pd
from pathlib import Path
from datetime import datetime
from scipy.stats import norm

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT     = Path(".")          # remplacez par le chemin absolu de votre projet
RAW_DIR  = ROOT / "data/cmip6/raw"
OUT_DIR  = ROOT / "data/cmip6/processed"
LOG_PATH = ROOT / "data/cmip6/compute_log.txt"
OUT_DIR.mkdir(parents=True, exist_ok=True)

MODELS = [
    "MIROC6",
    "CanESM5",
    "IPSL-CM6A-LR",
    "CNRM-CM6-1",
    "HadGEM3-GC31-LL",
    "MRI-ESM2-0",
    "BCC-CSM2-MR",
    "GFDL-ESM4",
    "ACCESS-ESM1-5",
    "NorESM2-LM",
]
EXPERIMENTS  = ["historical", "hist-nat"]
YEAR_START   = 1950
YEAR_END     = 2013
YEAR_STD_END = 2010
ACC_SCALE    = 12

# ── HELPERS ───────────────────────────────────────────────────────────────────
LOG_LINES = []

def log(msg=""):
    ts   = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    line = f"{ts} {msg}"
    print(line)
    LOG_LINES.append(line)

def load_variable(model, experiment, variable):
    pattern = f"{variable}_Amon_{model}_{experiment}_*.nc"
    files   = sorted(RAW_DIR.glob(pattern))
    files   = [f for f in files if "_Amon_" in f.name]
    if not files:
        raise FileNotFoundError(f"Aucun fichier trouvé : {pattern}")
    log(f"    {variable} : {len(files)} fichier(s)")
    ds = xr.open_mfdataset(files, combine="by_coords",
                            engine="netcdf4", use_cftime=True)
    return ds

def hargreaves_samani(tas_c, lat_rad, times):
    """
    ETP Hargreaves-Samani (1985).
    Amplitude thermique diurne (DTR) fixée à 12°C faute de tasmax/tasmin.
    Ra calculée analytiquement depuis la latitude et le jour de l'année.
    """
    doy  = np.array([pd.Timestamp(str(t.values)[:10]).dayofyear for t in times])
    dr   = 1 + 0.033 * np.cos(2 * np.pi * doy / 365)
    decl = 0.409 * np.sin(2 * np.pi * doy / 365 - 1.39)
    lat_2d = lat_rad
    arg    = np.clip(-np.tan(lat_2d) * np.tan(decl[:, None, None]), -1, 1)
    ws     = np.arccos(arg)
    Ra     = (24 * 60 / np.pi) * 0.082 * dr[:, None, None] * (
              ws * np.sin(lat_2d) * np.sin(decl[:, None, None]) +
              np.cos(lat_2d) * np.cos(decl[:, None, None]) * np.sin(ws)
             )
    days_per_month = np.array([pd.Timestamp(str(t.values)[:7] + "-01").days_in_month
                                for t in times])
    DTR  = 12.0   # amplitude thermique diurne fixée (approximation)
    etp  = 0.0023 * Ra * (tas_c[:, None, None] if tas_c.ndim == 1
                           else tas_c) * DTR**0.5
    etp  = etp / 2.45 * days_per_month[:, None, None]
    return np.maximum(etp, 0)

def fit_loglogistic_3p(x):
    """
    Ajuste une loi log-logistique à 3 paramètres (L-moments).
    Utilise math.gamma (np.math supprimé en NumPy 2.x).
    """
    gamma   = np.min(x) - 1e-6
    x_shift = x - gamma
    x_sort  = np.sort(x_shift)
    n       = len(x_sort)
    b0 = np.mean(x_sort)
    b1 = np.mean([i / (n - 1) * x_sort[i] for i in range(n)])
    b2 = np.mean([(i * (i - 1)) / ((n - 1) * (n - 2)) * x_sort[i]
                   for i in range(n)])
    beta  = (2 * b1 - b0) / (6 * b1 - b0 - 6 * b2)
    alpha = (b0 - 2 * b1) * beta / (math.gamma(1 + 1/beta) *
                                     math.gamma(1 - 1/beta))
    return alpha, beta, gamma

def compute_spei12(pr_mm, etp_mm, times, std_mask):
    D    = pr_mm - etp_mm
    nt, nlat, nlon = D.shape

    D_acc = np.full_like(D, np.nan)
    for t in range(ACC_SCALE - 1, nt):
        D_acc[t] = np.sum(D[t - ACC_SCALE + 1:t + 1], axis=0)

    spei      = np.full_like(D_acc, np.nan)
    month_idx = np.array([pd.Timestamp(str(t.values)[:10]).month for t in times])

    for m in range(1, 13):
        m_mask     = (month_idx == m)
        m_std_mask = m_mask & std_mask
        idx_all    = np.where(m_mask)[0]
        idx_std    = np.where(m_std_mask)[0]

        for i in range(nlat):
            for j in range(nlon):
                vals_std = D_acc[idx_std, i, j]
                vals_std = vals_std[~np.isnan(vals_std)]
                if len(vals_std) < 10:
                    continue
                try:
                    alpha, beta, gamma = fit_loglogistic_3p(vals_std)
                    vals_all = D_acc[idx_all, i, j]
                    x_shift  = vals_all - gamma
                    cdf = 1 / (1 + (alpha / np.maximum(x_shift, 1e-10))**beta)
                    cdf = np.clip(cdf, 1e-6, 1 - 1e-6)
                    spei[idx_all, i, j] = norm.ppf(cdf)
                except Exception:
                    pass
    return spei

# ── NETTOYAGE FICHIERS EXISTANTS ──────────────────────────────────────────────
log("\nNettoyage des fichiers SPEI existants (potentiellement corrompus)...")
for f in OUT_DIR.glob("spei12_*.nc"):
    f.unlink()
    log(f"  Supprimé : {f.name}")

# ── MAIN ──────────────────────────────────────────────────────────────────────
log("=" * 70)
log("001_compute_spei_cmip6.py — Calcul SPEI-12 CMIP6")
log(f"Modèles     : {MODELS}")
log(f"Expériences : {EXPERIMENTS}")
log(f"Fenêtre     : {YEAR_START}–{YEAR_END} | Std : {YEAR_START}–{YEAR_STD_END}")
log("=" * 70)

n_ok, n_err = 0, 0

for model in MODELS:
    for experiment in EXPERIMENTS:
        tag      = f"{model} / {experiment}"
        out_path = OUT_DIR / f"spei12_{model}_{experiment}.nc"

        log(f"\n{'='*60}")
        log(f"Traitement : {tag}")

        if out_path.exists():
            log(f"  SKIP (déjà présent) : {out_path.name}")
            n_ok += 1
            continue

        try:
            log("  [1/5] Lecture pr et tas")
            ds_pr  = load_variable(model, experiment, "pr")
            ds_tas = load_variable(model, experiment, "tas")

            log(f"  [2/5] Recadrage {YEAR_START}–{YEAR_END}")
            pr  = ds_pr["pr"].sel(time=slice(str(YEAR_START), str(YEAR_END)))
            tas = ds_tas["tas"].sel(time=slice(str(YEAR_START), str(YEAR_END)))

            common_times = np.intersect1d(pr.time.values, tas.time.values)
            pr  = pr.sel(time=common_times)
            tas = tas.sel(time=common_times)
            log(f"    {len(common_times)} pas de temps communs")

            log("  [3/5] Conversions unités")
            days   = np.array([pd.Timestamp(str(t)[:7] + "-01").days_in_month
                                for t in common_times])
            pr_mm  = pr.values * 86400 * days[:, None, None]
            tas_c  = tas.values - 273.15
            lat_rad = np.deg2rad(pr.lat.values)[:, None]

            log("  [4/5] Calcul ETP Hargreaves-Samani")
            etp_mm = hargreaves_samani(tas_c, lat_rad, pr.time)

            log("  [5/5] Calcul SPEI-12")
            times      = pr.time
            time_years = np.array([pd.Timestamp(str(t)[:10]).year
                                    for t in times.values])
            std_mask   = time_years <= YEAR_STD_END
            spei_arr   = compute_spei12(pr_mm, etp_mm, times, std_mask)

            n_valid = int(np.isfinite(spei_arr).sum())
            log(f"    Valeurs finies : {n_valid:,} / {spei_arr.size:,}")

            ds_out = xr.Dataset(
                {"spei12": (["time", "lat", "lon"], spei_arr)},
                coords={
                    "time": times.values,
                    "lat":  pr.lat.values,
                    "lon":  pr.lon.values,
                },
                attrs={
                    "model":      model,
                    "experiment": experiment,
                    "method":     "Hargreaves-Samani DTR=12C",
                    "std_period": f"{YEAR_START}-{YEAR_STD_END}",
                    "created":    datetime.now().isoformat(),
                }
            )
            ds_out.to_netcdf(out_path)
            log(f"  → Sauvegardé : {out_path.name}")
            n_ok += 1

        except Exception as e:
            log(f"  ERREUR {tag} : {e}")
            n_err += 1
            import traceback
            log(traceback.format_exc())

log("")
log("=" * 70)
log(f"Fin — OK : {n_ok} | Erreurs : {n_err}")
log("=" * 70)

LOG_PATH.write_text("\n".join(LOG_LINES), encoding="utf-8")

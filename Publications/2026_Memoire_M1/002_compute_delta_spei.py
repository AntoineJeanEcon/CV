# -*- coding: utf-8 -*-
"""
002_compute_delta_spei.py
=========================
Calcule le delta anthropogénique du SPEI-12 pour chaque modèle CMIP6.

Logique :
  Δ SPEI_it = SPEI12_hist_it - SPEI12_hist-nat_it

Le delta est calculé sur la grille native de chaque modèle, puis agrégé
annuellement via le minimum annuel (cohérence avec D_it = min(spei_min, 0)
utilisé dans le panel d'estimation).

Le delta multi-modèle est ensuite moyenné (mean ensemble) sur une grille
commune 2.5° × 2.5° (cohérence avec SPEIbase v2.10).

Fenêtre d'attribution : 1993–2013 (fenêtre d'estimation du panel)

Sources
-------
data/cmip6/processed/spei12_{model}_historical.nc
data/cmip6/processed/spei12_{model}_hist-nat.nc

Sortie
------
data/cmip6/processed/delta_spei_{model}.nc     (par modèle, annuel)
data/cmip6/processed/delta_spei_ensemble.nc    (moyenne multi-modèle)
data/cmip6/delta_log.txt
"""

import numpy as np
import xarray as xr
import pandas as pd
from pathlib import Path
from datetime import datetime

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT     = Path(".")          # remplacez par le chemin absolu de votre projet
PROC_DIR = ROOT / "data/cmip6/processed"
LOG_PATH = ROOT / "data/cmip6/delta_log.txt"

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

YEAR_START = 1993
YEAR_END   = 2013

REF_LAT = np.arange(-90, 90 + 2.5, 2.5)
REF_LON = np.arange(0, 360, 2.5)

# ── HELPERS ───────────────────────────────────────────────────────────────────
LOG_LINES = []

def log(msg=""):
    ts   = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    line = f"{ts} {msg}"
    print(line)
    LOG_LINES.append(line)

def spei_annual_min(ds, year_start, year_end):
    da = ds["spei12"].sel(time=slice(str(year_start), str(year_end)))
    return da.groupby("time.year").min(dim="time")

# ── MAIN ──────────────────────────────────────────────────────────────────────
log("=" * 70)
log("002_compute_delta_spei.py — Delta anthropogénique SPEI-12")
log(f"Modèles     : {MODELS}")
log(f"Fenêtre     : {YEAR_START}–{YEAR_END}")
log(f"Grille cible: 2.5° × 2.5° ({len(REF_LAT)} lat × {len(REF_LON)} lon)")
log("=" * 70)

n_ok, n_err   = 0, 0
deltas_regrid = []

for model in MODELS:
    log(f"\n{'='*60}")
    log(f"Traitement : {model}")

    out_path = PROC_DIR / f"delta_spei_{model}.nc"

    if out_path.exists():
        log(f"  SKIP calcul (déjà présent) : {out_path.name}")
        ds_delta = xr.open_dataset(out_path, engine="netcdf4")
        delta    = ds_delta["delta_spei"].load()
        ds_delta.close()
    else:
        hist_path    = PROC_DIR / f"spei12_{model}_historical.nc"
        histnat_path = PROC_DIR / f"spei12_{model}_hist-nat.nc"

        if not hist_path.exists() or not histnat_path.exists():
            log(f"  ERREUR : fichiers SPEI manquants pour {model}")
            n_err += 1
            continue

        try:
            log("  [1/3] Lecture historical et hist-nat")
            ds_hist    = xr.open_dataset(hist_path,    engine="netcdf4")
            ds_histnat = xr.open_dataset(histnat_path, engine="netcdf4")

            log("  [2/3] Agrégation annuelle (minimum)")
            da_hist    = spei_annual_min(ds_hist,    YEAR_START, YEAR_END).load()
            da_histnat = spei_annual_min(ds_histnat, YEAR_START, YEAR_END).load()

            ds_hist.close()
            ds_histnat.close()

            log(f"    hist    : {len(da_hist.year)} années")
            log(f"    hist-nat: {len(da_histnat.year)} années")

            years_common = np.intersect1d(da_hist.year.values,
                                           da_histnat.year.values)
            da_hist    = da_hist.sel(year=years_common)
            da_histnat = da_histnat.sel(year=years_common)
            log(f"    Années communes : {len(years_common)} ({years_common[0]}–{years_common[-1]})")

            log("  [3/3] Calcul delta = hist - hist-nat")
            delta = da_hist - da_histnat

            ds_out = xr.Dataset(
                {"delta_spei": delta},
                attrs={
                    "model":      model,
                    "definition": "SPEI12_historical - SPEI12_hist-nat, annual minimum",
                    "years":      f"{YEAR_START}-{YEAR_END}",
                    "created":    datetime.now().isoformat(),
                }
            )
            ds_out.to_netcdf(out_path)
            log(f"  → Sauvegardé : {out_path.name}")
            n_ok += 1

        except Exception as e:
            log(f"  ERREUR {model} : {e}")
            import traceback
            log(traceback.format_exc())
            n_err += 1
            continue

    try:
        log(f"  Regrid → grille 2.5° ({delta.lat.size} lat → {len(REF_LAT)})")
        da_r = delta.interp(lat=REF_LAT, lon=REF_LON, method="linear")
        deltas_regrid.append(da_r)
        log(f"  → Regrid OK : shape {da_r.shape}")
    except Exception as e:
        log(f"  ERREUR regrid {model} : {e}")
        n_err += 1

# ── ENSEMBLE MULTI-MODÈLE ─────────────────────────────────────────────────────
log(f"\n{'='*60}")
log(f"Calcul ensemble multi-modèle ({len(deltas_regrid)} modèles)")

ensemble_path = PROC_DIR / "delta_spei_ensemble.nc"
if ensemble_path.exists():
    ensemble_path.unlink()
    log("  Ancien fichier ensemble supprimé")

if len(deltas_regrid) > 0:
    try:
        stack    = xr.concat(deltas_regrid, dim="model")
        ensemble = stack.mean(dim="model")

        ds_ens = xr.Dataset(
            {"delta_spei": ensemble},
            attrs={
                "models":     str(MODELS),
                "n_models":   len(deltas_regrid),
                "definition": "Mean ensemble delta SPEI12 (hist - hist-nat), annual min",
                "grid":       "2.5deg",
                "years":      f"{YEAR_START}-{YEAR_END}",
                "created":    datetime.now().isoformat(),
            }
        )
        ds_ens.to_netcdf(ensemble_path)
        log(f"  → Ensemble sauvegardé : {ensemble_path.name}")
        log(f"    Shape  : {ensemble.shape}")
        log(f"    Delta moyen global (nanmean) : {float(np.nanmean(ensemble.values)):.4f}")
        log(f"    Delta min global   (nanmin)  : {float(np.nanmin(ensemble.values)):.4f}")
        log(f"    Delta max global   (nanmax)  : {float(np.nanmax(ensemble.values)):.4f}")

    except Exception as e:
        log(f"  ERREUR ensemble : {e}")
        import traceback
        log(traceback.format_exc())
else:
    log("  ERREUR : aucun delta disponible pour l'ensemble")

log("")
log("=" * 70)
log(f"Fin — OK : {n_ok} | Erreurs : {n_err}")
log("=" * 70)

LOG_PATH.write_text("\n".join(LOG_LINES), encoding="utf-8")

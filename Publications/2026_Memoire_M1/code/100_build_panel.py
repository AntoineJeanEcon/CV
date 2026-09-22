# -*- coding: utf-8 -*-
"""
100_build_panel.py
==================
Construit le fichier canonique `panel_canonical.csv` à partir des sources
brutes. Aucune référence à un N pré-existant : on prend ce que le pipeline
produit.

Convention de filtrage : strictement celle de Callahan & Mankin (2022),
vérifiée dans leurs scripts publics (ExtremeHeat_Growth_Regression.R, ligne 28
et 85 ; Construct_Extremes_Growth_Panel.py, ligne 349).

  → drop_na(growth)              (R, l.28)
  → filter(T_it != 0)            (R, l.85, retire les erreurs de mesure)
  → merge gauche PIB × climat    (Python, l.210)
  → pas de filtre |growth| <= x
  → pas d'exclusion de régions à PIB incertain
  → Océanie conservée

Sources brutes attendues
------------------------
1. data/raw/gdp.csv  (PIB downscalé Callahan & Mankin 2022, fenêtre 1992-2013)
     Colonnes : region, time, iso, gdp_percapita_mean, gdp_percapita_std

2. data/processed/climate_annual.csv  (sorties SPEI + ERA5 agrégées par GID_1)
     Colonnes requises : region_id, year, spei_mean, spei_min, T_it, t_var

Sortie
------
data/processed/panel_canonical.csv  (panel complet, NaN conservés)
data/processed/panel_estimation.csv (échantillon d'estimation filtré)
data/processed/waterfall.txt        (log du waterfall)
"""

from pathlib import Path
import sys
import pandas as pd
import numpy as np

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT = Path(".")              # remplacez par le chemin absolu de votre projet
GDP_PATH         = ROOT / "data/raw/gdp.csv"
CLIMATE_PATH     = ROOT / "data/processed/climate_annual.csv"
OUT_CANONICAL    = ROOT / "data/processed/panel_canonical.csv"
OUT_ESTIMATION   = ROOT / "data/processed/panel_estimation.csv"
OUT_LOG          = ROOT / "data/processed/waterfall.txt"

YEAR_START, YEAR_END = 1992, 2013
N_LAGS = 4

# ── HELPERS ───────────────────────────────────────────────────────────────────
LOG_LINES = []
def log(msg=""):
    print(msg)
    LOG_LINES.append(msg)

def step(label, mask, df):
    sub = df[mask]
    log(f"  {label:<60} {len(sub):>10,} obs  {sub['region_id'].nunique():>5} reg")
    return sub

# ── 1. LECTURE PIB ────────────────────────────────────────────────────────────
log("=" * 80)
log("100_build_panel.py — panel canonique (règles C&M 2022 vérifiées)")
log("=" * 80)

if not GDP_PATH.exists():
    sys.exit(f"ERREUR: {GDP_PATH} introuvable.")

log(f"\n[1/5] Lecture PIB : {GDP_PATH.name}")
gdp = pd.read_csv(GDP_PATH)
gdp = gdp.rename(columns={"region": "region_id", "time": "year"})
gdp = gdp[["region_id", "year", "iso", "gdp_percapita_mean", "gdp_percapita_std"]]
gdp["year"] = gdp["year"].astype(int)
log(f"      {len(gdp):,} lignes | {gdp['region_id'].nunique():,} régions"
    f" | {gdp['year'].min()}-{gdp['year'].max()}")

# ── 2. LECTURE CLIMAT ─────────────────────────────────────────────────────────
log(f"\n[2/5] Lecture climat : {CLIMATE_PATH.name}")
if not CLIMATE_PATH.exists():
    spei_path = ROOT / "data/processed/spei_annual.csv"
    era5_path = ROOT / "data/processed/era5_annual.csv"
    if not (spei_path.exists() and era5_path.exists()):
        sys.exit(f"ERREUR: ni {CLIMATE_PATH.name} ni les fichiers SPEI/ERA5 séparés.")
    spei = pd.read_csv(spei_path).rename(columns={"GID_1": "region_id"})
    era5 = pd.read_csv(era5_path).rename(columns={"GID_1": "region_id",
                                                   "t_mean": "T_it"})
    clim = spei.merge(era5, on=["region_id", "year"], how="outer")
    log("      (reconstruit depuis spei_annual.csv + era5_annual.csv)")
else:
    clim = pd.read_csv(CLIMATE_PATH)

required = {"region_id", "year", "spei_mean", "spei_min", "T_it", "t_var"}
missing  = required - set(clim.columns)
if missing:
    sys.exit(f"ERREUR: colonnes climat manquantes : {missing}")
clim["year"] = clim["year"].astype(int)
log(f"      {len(clim):,} lignes | {clim['region_id'].nunique():,} régions"
    f" | {clim['year'].min()}-{clim['year'].max()}")

# ── 3. MERGE ──────────────────────────────────────────────────────────────────
log(f"\n[3/5] Merge climat × PIB (left sur climat, suivant C&M l.210)")
panel = clim.merge(gdp, on=["region_id", "year"], how="left")
panel = panel[(panel["year"] >= YEAR_START) & (panel["year"] <= YEAR_END)]
panel = panel.sort_values(["region_id", "year"]).reset_index(drop=True)
log(f"      {len(panel):,} lignes après filtre temporel {YEAR_START}-{YEAR_END}")

# ── 4. VARIABLES DÉRIVÉES ─────────────────────────────────────────────────────
log(f"\n[4/5] Construction variables dérivées")

panel["gdp_lag"] = panel.groupby("region_id")["gdp_percapita_mean"].shift(1)
panel["growth"]  = (panel["gdp_percapita_mean"] - panel["gdp_lag"]) / panel["gdp_lag"]
panel["log_gdp"] = np.log(panel["gdp_percapita_mean"])
panel = panel.drop(columns=["gdp_lag"])

# Chocs hydriques
panel["D_v1"]   = panel["spei_min"]
panel["D_v2"]   = (panel["spei_min"] < -1.5).astype("Float64")
panel.loc[panel["spei_min"].isna(), "D_v2"] = np.nan
panel["D_v3"]   = -np.minimum(panel["spei_min"], 0)   # spécification principale
panel["S_plus"] = np.maximum(panel["spei_mean"], 0)   # excédent hydrique (spec b)

# Modérateurs long terme (constants par région)
panel["T_bar_temp"] = panel.groupby("region_id")["T_it"].transform("mean")
panel["T_bar_spei"] = panel.groupby("region_id")["spei_mean"].transform("mean")

# Lags pour DLM
for k in range(1, N_LAGS + 1):
    for v in ("D_v1", "D_v2", "D_v3"):
        panel[f"{v}_lag{k}"] = panel.groupby("region_id")[v].shift(k)

log(f"      growth, D_v1/v2/v3, T_bar_temp, T_bar_spei, lags 1-{N_LAGS}")

# ── 5. WATERFALL ──────────────────────────────────────────────────────────────
log(f"\n[5/5] Waterfall de construction de l'échantillon d'estimation")
log(f"      (règles C&M : drop_na(growth), filter(T_it != 0))\n")
log(f"  {'Étape':<60} {'N obs':>10}  {'Reg':>5}")
log(f"  {'-'*82}")

m_all = pd.Series(True, index=panel.index)
step("A. Panel brut (climat × PIB, 1992-2013)", m_all, panel)

m_b = panel["gdp_percapita_mean"].notna()
step("B. + PIB non-nul", m_b, panel)

m_c = m_b & panel["growth"].notna()
step("C. + growth calculable (dropna, suivant C&M R l.28)", m_c, panel)

m_d = m_c & (panel["T_it"] != 0) & panel["T_it"].notna()
step("D. + T_it != 0 (suivant C&M R l.85)", m_d, panel)

m_e = m_d & panel["D_v3"].notna()
step("E. + D_v3 défini (spei_min observé)", m_e, panel)

covars = ["spei_mean", "t_var"]
m_f = m_e & panel[covars].notna().all(axis=1)
estim = step("F. + spei_mean & t_var [ÉCHANTILLON D'ESTIMATION]", m_f, panel)

log("")
log(f"  Année de départ effective : {estim['year'].min()}")
log(f"  Fenêtre d'estimation      : {estim['year'].min()}-{estim['year'].max()}")

# ── 6. SAUVEGARDE ─────────────────────────────────────────────────────────────
OUT_CANONICAL.parent.mkdir(parents=True, exist_ok=True)
panel.to_csv(OUT_CANONICAL, index=False)
estim.to_csv(OUT_ESTIMATION, index=False)
OUT_LOG.write_text("\n".join(LOG_LINES), encoding="utf-8")

log(f"\nSauvegardes :")
log(f"  {OUT_CANONICAL.name:<30} (panel complet, NaN conservés)")
log(f"  {OUT_ESTIMATION.name:<30} (échantillon d'estimation)")
log(f"  {OUT_LOG.name:<30} (log du waterfall)")
log("=" * 80)

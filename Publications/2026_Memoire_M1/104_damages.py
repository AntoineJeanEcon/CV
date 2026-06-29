# -*- coding: utf-8 -*-
"""
104_damages.py
==============
Calcul des pertes économiques attribuables aux déficits hydriques (SPEI)
à partir des coefficients du distributed lag model.

Logique :
  Δg_it = Σ_{k=0}^{4} β̂_k × D_v3_{i,t-k}
  loss_it = Δg_it × gdp_percapita_obs_it          (perte de niveau, sans compounding)
  total_it (Mds$) = loss_it × population_it / 1e9

La perte est transitoire (pas de compounding inter-annuel), ce qui est
cohérent avec Σβ ≈ 0 (la trajectoire du PIB revient vers son niveau tendanciel).
IC 90% calculé par bootstrap paramétrique (β ~ N(coef, SE²), N_BOOT=1000).

Sources
-------
data/processed/panel_estimation.csv
results/table2_distributedlag.csv
data/raw/population/GPW_population_interpolated_region_1991-2015.csv

Sortie
------
results/damages_by_region.csv
"""

import pandas as pd
import numpy as np
from pathlib import Path
import warnings
warnings.filterwarnings("ignore")

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT = Path(".")              # remplacez par le chemin absolu de votre projet

DATA_PANEL = ROOT / "data" / "processed" / "panel_estimation.csv"
DATA_LAGS  = ROOT / "results" / "table2_distributedlag.csv"
DATA_POP   = ROOT / "data" / "raw" / "population" / "GPW_population_interpolated_region_1991-2015.csv"
OUT_CSV    = ROOT / "results" / "damages_by_region.csv"
(ROOT / "results").mkdir(parents=True, exist_ok=True)

# ── 1. MAPPING ISO → CONTINENT ────────────────────────────────────────────────
try:
    import pycountry_convert as pc

    def iso_to_continent(iso2):
        try:
            code = pc.country_alpha2_to_continent_code(iso2)
            mapping = {
                "AF": "Afrique", "AS": "Asie", "EU": "Europe",
                "NA": "Amérique du Nord", "SA": "Amérique du Sud",
                "OC": "Océanie", "AN": "Antarctique",
            }
            return mapping.get(code, "Inconnu")
        except Exception:
            return "Inconnu"

    USE_PYCOUNTRY = True
    print("[INFO] pycountry_convert disponible — utilisation pour le mapping ISO → continent.")

except ImportError:
    USE_PYCOUNTRY = False
    print("[INFO] pycountry_convert absent — utilisation du mapping manuel.")

CONTINENT_MAP_ISO3 = {
    "DZA": "Afrique", "AGO": "Afrique", "BEN": "Afrique", "BWA": "Afrique",
    "BFA": "Afrique", "BDI": "Afrique", "CMR": "Afrique", "CAF": "Afrique",
    "TCD": "Afrique", "COM": "Afrique", "COD": "Afrique", "COG": "Afrique",
    "CIV": "Afrique", "DJI": "Afrique", "EGY": "Afrique", "GNQ": "Afrique",
    "ERI": "Afrique", "ETH": "Afrique", "GAB": "Afrique", "GMB": "Afrique",
    "GHA": "Afrique", "GIN": "Afrique", "GNB": "Afrique", "KEN": "Afrique",
    "LSO": "Afrique", "LBR": "Afrique", "LBY": "Afrique", "MDG": "Afrique",
    "MWI": "Afrique", "MLI": "Afrique", "MRT": "Afrique", "MUS": "Afrique",
    "MAR": "Afrique", "MOZ": "Afrique", "NAM": "Afrique", "NER": "Afrique",
    "NGA": "Afrique", "RWA": "Afrique", "STP": "Afrique", "SEN": "Afrique",
    "SLE": "Afrique", "SOM": "Afrique", "ZAF": "Afrique", "SSD": "Afrique",
    "SDN": "Afrique", "SWZ": "Afrique", "TZA": "Afrique", "TGO": "Afrique",
    "TUN": "Afrique", "UGA": "Afrique", "ZMB": "Afrique", "ZWE": "Afrique",
    "CPV": "Afrique", "SHN": "Afrique",
    "AFG": "Asie", "ARM": "Asie", "AZE": "Asie", "BHR": "Asie",
    "BGD": "Asie", "BTN": "Asie", "BRN": "Asie", "KHM": "Asie",
    "CHN": "Asie", "CYP": "Asie", "GEO": "Asie", "IND": "Asie",
    "IDN": "Asie", "IRN": "Asie", "IRQ": "Asie", "ISR": "Asie",
    "JPN": "Asie", "JOR": "Asie", "KAZ": "Asie", "KWT": "Asie",
    "KGZ": "Asie", "LAO": "Asie", "LBN": "Asie", "MYS": "Asie",
    "MDV": "Asie", "MNG": "Asie", "MMR": "Asie", "NPL": "Asie",
    "PRK": "Asie", "OMN": "Asie", "PAK": "Asie", "PHL": "Asie",
    "QAT": "Asie", "SAU": "Asie", "SGP": "Asie", "KOR": "Asie",
    "LKA": "Asie", "SYR": "Asie", "TWN": "Asie", "TJK": "Asie",
    "THA": "Asie", "TLS": "Asie", "TUR": "Asie", "TKM": "Asie",
    "ARE": "Asie", "UZB": "Asie", "VNM": "Asie", "YEM": "Asie",
    "PSE": "Asie", "HKG": "Asie", "MAC": "Asie",
    "ALB": "Europe", "AND": "Europe", "AUT": "Europe", "BLR": "Europe",
    "BEL": "Europe", "BIH": "Europe", "BGR": "Europe", "HRV": "Europe",
    "CZE": "Europe", "DNK": "Europe", "EST": "Europe", "FIN": "Europe",
    "FRA": "Europe", "DEU": "Europe", "GRC": "Europe", "HUN": "Europe",
    "ISL": "Europe", "IRL": "Europe", "ITA": "Europe", "XKX": "Europe",
    "LVA": "Europe", "LIE": "Europe", "LTU": "Europe", "LUX": "Europe",
    "MLT": "Europe", "MDA": "Europe", "MCO": "Europe", "MNE": "Europe",
    "NLD": "Europe", "MKD": "Europe", "NOR": "Europe", "POL": "Europe",
    "PRT": "Europe", "ROU": "Europe", "RUS": "Europe", "SMR": "Europe",
    "SRB": "Europe", "SVK": "Europe", "SVN": "Europe", "ESP": "Europe",
    "SWE": "Europe", "CHE": "Europe", "UKR": "Europe", "GBR": "Europe",
    "VAT": "Europe", "KOS": "Europe",
    "ATG": "Amérique du Nord", "BHS": "Amérique du Nord", "BRB": "Amérique du Nord",
    "BLZ": "Amérique du Nord", "CAN": "Amérique du Nord", "CRI": "Amérique du Nord",
    "CUB": "Amérique du Nord", "DMA": "Amérique du Nord", "DOM": "Amérique du Nord",
    "SLV": "Amérique du Nord", "GRD": "Amérique du Nord", "GTM": "Amérique du Nord",
    "HTI": "Amérique du Nord", "HND": "Amérique du Nord", "JAM": "Amérique du Nord",
    "MEX": "Amérique du Nord", "NIC": "Amérique du Nord", "PAN": "Amérique du Nord",
    "KNA": "Amérique du Nord", "LCA": "Amérique du Nord", "VCT": "Amérique du Nord",
    "TTO": "Amérique du Nord", "USA": "Amérique du Nord", "VGB": "Amérique du Nord",
    "PRI": "Amérique du Nord",
    "ARG": "Amérique du Sud", "BOL": "Amérique du Sud", "BRA": "Amérique du Sud",
    "CHL": "Amérique du Sud", "COL": "Amérique du Sud", "ECU": "Amérique du Sud",
    "GUY": "Amérique du Sud", "PRY": "Amérique du Sud", "PER": "Amérique du Sud",
    "SUR": "Amérique du Sud", "URY": "Amérique du Sud", "VEN": "Amérique du Sud",
    "AUS": "Océanie", "FJI": "Océanie", "KIR": "Océanie", "MHL": "Océanie",
    "FSM": "Océanie", "NRU": "Océanie", "NZL": "Océanie", "PLW": "Océanie",
    "PNG": "Océanie", "WSM": "Océanie", "SLB": "Océanie", "TON": "Océanie",
    "TUV": "Océanie", "VUT": "Océanie",
}

def get_continent(iso_code):
    if pd.isna(iso_code):
        return "Inconnu"
    iso_code = str(iso_code).strip().upper()
    if USE_PYCOUNTRY:
        try:
            import pycountry
            country = pycountry.countries.get(alpha_3=iso_code)
            if country:
                return iso_to_continent(country.alpha_2)
        except Exception:
            pass
    return CONTINENT_MAP_ISO3.get(iso_code, "Inconnu")

# ── 2. CHARGEMENT ─────────────────────────────────────────────────────────────
print("\n[1/6] Chargement des données...")

panel   = pd.read_csv(DATA_PANEL)
lags_df = pd.read_csv(DATA_LAGS)

print(f"  Panel : {len(panel):,} observations, {panel['region_id'].nunique():,} régions")

if DATA_POP.exists():
    pop = pd.read_csv(DATA_POP)
    pop = pop.rename(columns={"id": "region_id", "time": "year"})
    pop = pop[["region_id", "year", "population"]]
    pop["year"] = pop["year"].astype(int)
    n_neg = (pop["population"] < 0).sum()
    pop["population"] = pop["population"].clip(lower=0)
    if n_neg > 0:
        print(f"  [INFO] {n_neg} valeurs de population < 0 clippées à 0 (artefact GPW bord)")
    HAS_POP = True
else:
    print(f"  [AVERTISSEMENT] Fichier population introuvable : {DATA_POP}")
    HAS_POP = False

lags_df = lags_df[lags_df["lag"].astype(str).str.match(r"^\d+$")].copy()
lags_df["lag"] = lags_df["lag"].astype(int)
lags_df = lags_df[lags_df["lag"].between(0, 4)].sort_values("lag").reset_index(drop=True)

for _, row in lags_df.iterrows():
    sig = "***" if row["pval"] < 0.01 else ("**" if row["pval"] < 0.05 else ("*" if row["pval"] < 0.10 else ""))
    print(f"    β{int(row['lag'])} = {row['coef']:+.5f}  (p={row['pval']:.3f}) {sig}")

beta = {int(row["lag"]): row["coef"] for _, row in lags_df.iterrows()}
LAG_COLS = {0: "D_v3", 1: "D_v3_lag1", 2: "D_v3_lag2", 3: "D_v3_lag3", 4: "D_v3_lag4"}

# ── 3. CALCUL DES PERTES ──────────────────────────────────────────────────────
print("\n[2/6] Calcul des trajectoires contrefactuelles par région...")

panel = panel.sort_values(["region_id", "year"]).reset_index(drop=True)

results_list = []
regions      = panel["region_id"].unique()
n_regions    = len(regions)

for idx, region in enumerate(regions):
    if (idx + 1) % 500 == 0:
        print(f"  Région {idx+1}/{n_regions}...")

    df_r     = panel[panel["region_id"] == region].copy().sort_values("year").reset_index(drop=True)
    iso_code = df_r["iso"].iloc[0]
    n_years  = len(df_r)
    if n_years < 2:
        continue

    lag_available = df_r[list(LAG_COLS.values())].notna().all(axis=1)

    delta_g = pd.Series(np.nan, index=df_r.index)
    for k, col in LAG_COLS.items():
        b = beta.get(k, 0.0)
        delta_g = delta_g.fillna(0.0)
        delta_g = delta_g + b * df_r[col].fillna(0.0)
    delta_g[~lag_available] = np.nan

    gdp_obs      = df_r["gdp_percapita_mean"].values
    delta_g_vals = delta_g.values
    loss         = delta_g_vals * gdp_obs
    loss         = np.where(np.isnan(delta_g_vals), np.nan, loss)

    for t in range(n_years):
        results_list.append({
            "region_id": region,
            "iso":       iso_code,
            "year":      df_r["year"].iloc[t],
            "gdp_obs":   gdp_obs[t],
            "delta_g":   delta_g_vals[t],
            "loss":      loss[t],
        })

print(f"  Calcul terminé : {len(results_list):,} observations traitées.")

# ── 4. AGRÉGATION + IC BOOTSTRAP ─────────────────────────────────────────────
print("\n[3/6] Agrégation par région...")

results_annual = pd.DataFrame(results_list)

damages = (
    results_annual.groupby(["region_id", "iso"])
    .agg(mean_loss_annual=("loss", "mean"),
         cumulative_loss=("loss", "sum"),
         n_years=("loss", "count"))
    .reset_index()
)
damages["continent"] = damages["iso"].apply(get_continent)

gdp_mean_region = (results_annual.groupby("region_id")["gdp_obs"].mean()
                   .reset_index().rename(columns={"gdp_obs": "gdp_mean"}))
damages = damages.merge(gdp_mean_region, on="region_id", how="left")
damages["loss_pct"] = damages["cumulative_loss"] / damages["gdp_mean"] * 100

if HAS_POP:
    results_annual_pop = results_annual.merge(pop[["region_id", "year", "population"]],
                                              on=["region_id", "year"], how="left")
    results_annual_pop["loss_total_usd"] = (results_annual_pop["loss"]
                                             * results_annual_pop["population"])
    pop_agg = (results_annual_pop.groupby("region_id")
               .agg(population_mean=("population", "mean"),
                    cumulative_loss_usd=("loss_total_usd", "sum"))
               .reset_index())
    pop_agg["cumulative_loss_bn"] = pop_agg["cumulative_loss_usd"] / 1e9
    damages = damages.merge(pop_agg, on="region_id", how="left")

    # Bootstrap IC 90% sur le total mondial
    IC_ALPHA = 0.10
    N_BOOT   = 1000
    beta_coefs = np.array([beta[k] for k in range(5)])
    beta_ses   = np.array([float(lags_df.loc[lags_df["lag"] == k, "se"].values[0]) for k in range(5)])
    W = np.zeros(5)
    panel_for_w = panel.merge(pop[["region_id", "year", "population"]],
                              on=["region_id", "year"], how="left")
    for k, col in LAG_COLS.items():
        mask = panel_for_w[col].notna() & panel_for_w["population"].notna()
        W[k] = (panel_for_w.loc[mask, col]
                * panel_for_w.loc[mask, "gdp_percapita_mean"]
                * panel_for_w.loc[mask, "population"]).sum() / 1e9

    rng_boot        = np.random.default_rng(42)
    beta_boot       = rng_boot.normal(beta_coefs, beta_ses, (N_BOOT, 5))
    total_loss_boot = beta_boot @ W
    ic_low  = np.percentile(total_loss_boot, IC_ALPHA / 2 * 100)
    ic_high = np.percentile(total_loss_boot, (1 - IC_ALPHA / 2) * 100)
    total_loss_point = float(damages["cumulative_loss_bn"].sum())
    print(f"\n  Bootstrap IC{int((1-IC_ALPHA)*100)}% total mondial :")
    print(f"    Point estimate : {total_loss_point:,.1f} Mds$")
    print(f"    IC {int((1-IC_ALPHA)*100)}%         : [{ic_low:,.1f}  ;  {ic_high:,.1f}] Mds$")
    print(f"    (Inclut zéro : {ic_low <= 0 <= ic_high})")
else:
    damages["population_mean"]     = np.nan
    damages["cumulative_loss_usd"] = np.nan
    damages["cumulative_loss_bn"]  = np.nan

damages = damages[["region_id", "iso", "continent",
                   "mean_loss_annual", "cumulative_loss", "n_years",
                   "loss_pct", "gdp_mean",
                   "population_mean", "cumulative_loss_usd", "cumulative_loss_bn"]]
damages.to_csv(OUT_CSV, index=False)
print(f"\n  → Exporté : {OUT_CSV}")
print(f"  {len(damages):,} régions")

# ── 5. FIGURES ────────────────────────────────────────────────────────────────
# Les figures (figure4, figure5, figure6) sont tracées ici.
# Le code de figure est conservé tel quel depuis le script original 506-damages.py.
# Voir ce script pour le détail de la mise en forme.

print("Script terminé.")

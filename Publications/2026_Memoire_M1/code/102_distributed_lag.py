# -*- coding: utf-8 -*-
"""
102_distributed_lag.py
======================
Estimation du distributed lag model (équation 4.2) sur le choc D_v3.
IC 95% par deux méthodes :
  - Méthode delta (analytique, covariance clusterisée)
  - Bootstrap par bloc (régions entières, index-based, N_BOOT itérations)

Spécification :
  growth_it = Σ_{k=0}^{4} β_k D_v3_{i,t-k} + β_S S_plus_it + β_T T_it + β_t t_var_it
              + μ_i + λ_t + ε_it

Sources
-------
data/processed/panel_estimation.csv

Sortie
------
results/table2_distributedlag.csv
results/table2_dlm_checkpoint.csv  (checkpoint intermédiaire bootstrap)
"""

import sys
import time
from pathlib import Path
import numpy as np
import pandas as pd
from linearmodels.panel import PanelOLS
import warnings

warnings.filterwarnings("ignore")

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT      = Path(".")         # remplacez par le chemin absolu de votre projet
DATA_FILE = ROOT / "data" / "processed" / "panel_estimation.csv"
OUT_DIR   = ROOT / "results"
OUT_CSV   = OUT_DIR / "table2_distributedlag.csv"
CKPT_CSV  = OUT_DIR / "table2_dlm_checkpoint.csv"

OUT_DIR.mkdir(parents=True, exist_ok=True)

# ── 1. CHARGEMENT ─────────────────────────────────────────────────────────────
print("-" * 60)
print("Chargement des données...")
sys.stdout.flush()

df = pd.read_csv(DATA_FILE)

VARS_MODEL = [
    "growth",
    "D_v3", "D_v3_lag1", "D_v3_lag2", "D_v3_lag3", "D_v3_lag4",
    "S_plus", "T_it", "t_var",
    "region_id", "year",
]
df = df[VARS_MODEL].dropna().set_index(["region_id", "year"])

n_obs     = len(df)
n_regions = df.index.get_level_values("region_id").nunique()
n_years   = df.index.get_level_values("year").nunique()

print(f"  Observations : {n_obs:,}")
print(f"  Régions      : {n_regions:,}")
print(f"  Années       : {n_years}")
sys.stdout.flush()

# ── 2. FORMULE ET ESTIMATEUR ──────────────────────────────────────────────────
FORMULA = (
    "growth ~ 1 + D_v3 + D_v3_lag1 + D_v3_lag2 + D_v3_lag3 + D_v3_lag4"
    " + S_plus + T_it + t_var"
    " + EntityEffects + TimeEffects"
)
LAG_VARS   = ["D_v3", "D_v3_lag1", "D_v3_lag2", "D_v3_lag3", "D_v3_lag4"]
LAG_LABELS = ["beta0(lag0)", "beta1(lag1)", "beta2(lag2)", "beta3(lag3)", "beta4(lag4)"]

def estimer_dlm(panel_df):
    mod = PanelOLS.from_formula(FORMULA, data=panel_df)
    res = mod.fit(cov_type="clustered", cluster_entity=True, use_lsdv=False)
    return np.array([res.params[v] for v in LAG_VARS])

# ── 3. ESTIMATION PRINCIPALE ──────────────────────────────────────────────────
print("\nEstimation principale (TWFE + erreurs clusterisées)...")
sys.stdout.flush()

mod_main  = PanelOLS.from_formula(FORMULA, data=df)
res_main  = mod_main.fit(cov_type="clustered", cluster_entity=True, use_lsdv=False)

betas_main = np.array([res_main.params[v]      for v in LAG_VARS])
se_main    = np.array([res_main.std_errors[v]   for v in LAG_VARS])
pval_main  = np.array([res_main.pvalues[v]      for v in LAG_VARS])

print("  Estimation principale terminée.")
for k in range(5):
    print(f"  {LAG_LABELS[k]:14s}  beta={betas_main[k]:+.5f}  SE={se_main[k]:.5f}  p={pval_main[k]:.4f}")
sys.stdout.flush()

# ── 4. IC 95% PAR MÉTHODE DELTA ───────────────────────────────────────────────
print("\nCalcul IC 95% par méthode delta (Cov clusterisée)...")
sys.stdout.flush()

Z95 = 1.96
ci_low_delta  = betas_main - Z95 * se_main
ci_high_delta = betas_main + Z95 * se_main

cov_full = res_main.cov
lag_idx  = [list(res_main.params.index).index(v) for v in LAG_VARS]
cov_lags = cov_full.values[np.ix_(lag_idx, lag_idx)]

e               = np.ones(5)
var_cumul       = float(e @ cov_lags @ e)
se_cumul        = np.sqrt(var_cumul)
cumul_main      = float(betas_main.sum())
cumul_ci_low_d  = cumul_main - Z95 * se_cumul
cumul_ci_high_d = cumul_main + Z95 * se_cumul

print(f"  Cumul delta : {cumul_main:+.5f}  SE={se_cumul:.5f}"
      f"  IC95=[{cumul_ci_low_d:+.5f} ; {cumul_ci_high_d:+.5f}]")
sys.stdout.flush()

# ── 5. BOOTSTRAP PAR BLOC ─────────────────────────────────────────────────────
N_BOOT = 1000
SEED   = 42
rng    = np.random.default_rng(seed=SEED)

df_flat     = df.reset_index()
regions_all = df_flat["region_id"].unique()
n_reg       = len(regions_all)

region_pos = {}
for rid, grp in df_flat.groupby("region_id", sort=False):
    region_pos[rid] = grp.index.to_numpy()

boot_coefs = np.full((N_BOOT, 5), np.nan)
n_skip     = 0
t0         = time.time()

print(f"\nBootstrap bloc N={N_BOOT}, seed={SEED} (index-based, sans pd.concat)...")
print("  Progression toutes les 100 itérations.")
sys.stdout.flush()

for b in range(N_BOOT):
    sampled = rng.choice(regions_all, size=n_reg, replace=True)
    all_pos = np.concatenate([region_pos[r] for r in sampled])
    all_eid = np.concatenate(
        [np.full(len(region_pos[r]), i, dtype=np.int32) for i, r in enumerate(sampled)]
    )
    boot_flat  = df_flat.iloc[all_pos].copy()
    boot_flat  = boot_flat.assign(region_id=all_eid)
    boot_panel = boot_flat.set_index(["region_id", "year"])

    try:
        boot_coefs[b] = estimer_dlm(boot_panel)
    except Exception:
        n_skip += 1
        boot_coefs[b] = boot_coefs[b - 1] if b > 0 else betas_main

    if (b + 1) % 100 == 0:
        elapsed  = time.time() - t0
        per_iter = elapsed / (b + 1)
        rem_min  = per_iter * (N_BOOT - b - 1) / 60
        print(f"  iter {b+1:4d}/{N_BOOT}  "
              f"{elapsed/60:.1f}min écoulé  ~{rem_min:.1f}min restant  skip={n_skip}",
              flush=True)
        _ok  = boot_coefs[:b+1][~np.isnan(boot_coefs[:b+1]).any(axis=1)]
        if len(_ok) >= 10:
            _rows = [{"iter": b+1, "lag": k,
                      "ci_low_boot":  np.percentile(_ok, 2.5,  axis=0)[k],
                      "ci_high_boot": np.percentile(_ok, 97.5, axis=0)[k]}
                     for k in range(5)]
            _cum  = _ok.sum(axis=1)
            _rows.append({"iter": b+1, "lag": "cumul",
                          "ci_low_boot":  float(np.percentile(_cum, 2.5)),
                          "ci_high_boot": float(np.percentile(_cum, 97.5))})
            pd.DataFrame(_rows).to_csv(CKPT_CSV, index=False, encoding="utf-8")

valid_mask   = ~np.isnan(boot_coefs).any(axis=1)
boot_valid   = boot_coefs[valid_mask]
n_valid_boot = len(boot_valid)

ci_low_boot  = np.percentile(boot_valid, 2.5,  axis=0)
ci_high_boot = np.percentile(boot_valid, 97.5, axis=0)

cumul_boot      = boot_valid.sum(axis=1)
cumul_ci_low_b  = float(np.percentile(cumul_boot, 2.5))
cumul_ci_high_b = float(np.percentile(cumul_boot, 97.5))

print(f"\n  Bootstrap terminé : {n_valid_boot}/{N_BOOT} valides, {n_skip} skips")
if n_skip > N_BOOT * 0.02:
    print(f"  ATTENTION : {n_skip/N_BOOT*100:.1f}% itérations skippées > 2%")
print(f"  Cumul IC95 boot = [{cumul_ci_low_b:+.5f} ; {cumul_ci_high_b:+.5f}]")
sys.stdout.flush()

# ── 6. SAUVEGARDE CSV ─────────────────────────────────────────────────────────
rows = []
for k in range(5):
    rows.append({
        "lag":           k,
        "coef":          betas_main[k],
        "se":            se_main[k],
        "pval":          pval_main[k],
        "ci_low_delta":  ci_low_delta[k],
        "ci_high_delta": ci_high_delta[k],
        "ci_low_boot":   ci_low_boot[k],
        "ci_high_boot":  ci_high_boot[k],
    })
rows.append({
    "lag":           "cumul",
    "coef":          cumul_main,
    "se":            float("nan"),
    "pval":          float("nan"),
    "ci_low_delta":  cumul_ci_low_d,
    "ci_high_delta": cumul_ci_high_d,
    "ci_low_boot":   cumul_ci_low_b,
    "ci_high_boot":  cumul_ci_high_b,
})

pd.DataFrame(rows).to_csv(OUT_CSV, index=False, float_format="%.6f", encoding="utf-8")
print(f"\n==> Résultats exportés : {OUT_CSV}", flush=True)

# ── 7. AFFICHAGE CONSOLE ──────────────────────────────────────────────────────
print("\n" + "=" * 80)
print("DISTRIBUTED LAG MODEL — beta0 à beta4  (choc D_v3)")
print("=" * 80)
hdr = (f"{'Lag':<14} {'Coef':>10} {'SE':>10} {'p-val':>8}"
       f"  {'IC95 delta':>24}  {'IC95 boot':>24}")
print(hdr); print("-" * 80)

for k in range(5):
    ic_d = f"[{ci_low_delta[k]:+.5f};{ci_high_delta[k]:+.5f}]"
    ic_b = f"[{ci_low_boot[k]:+.5f};{ci_high_boot[k]:+.5f}]"
    print(f"{LAG_LABELS[k]:<14} {betas_main[k]:>+10.5f} {se_main[k]:>10.5f} "
          f"{pval_main[k]:>8.4f}  {ic_d:>26}  {ic_b:>26}")

print("-" * 80)
ic_d_c = f"[{cumul_ci_low_d:+.5f};{cumul_ci_high_d:+.5f}]"
ic_b_c = f"[{cumul_ci_low_b:+.5f};{cumul_ci_high_b:+.5f}]"
print(f"{'Cumul(sum_beta)':<14} {cumul_main:>+10.5f} {'':>10} {'':>8}  "
      f"{ic_d_c:>26}  {ic_b_c:>26}")
print("=" * 80)

print(f"\nR2 within   : {res_main.rsquared:.4f}")
print(f"R2 overall  : {res_main.rsquared_overall:.4f}")
print(f"N obs.      : {int(res_main.nobs):,}")
print(f"N entités   : {res_main.entity_info['total']:,.0f}")
print("-" * 60)

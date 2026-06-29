"""
105_robustness.py
=================
Tests de robustesse pour le modele de base V3 (TWFE).
  Test 1 -- Tendances lineaires region-specifiques (Frisch-Waugh)
  Test 2 -- Test placebo par permutation des series temporelles

Donnees : data/processed/panel_estimation.csv
Sortie  : results/table3_robustness.csv
          results/figures/figure3_placebo.png
Checkpoint : results/table3_placebo_checkpoint.csv
"""

import sys
import time
import gc
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import warnings
from pathlib import Path
from linearmodels.panel import PanelOLS

warnings.filterwarnings("ignore")

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT = Path(".")              # remplacez par le chemin absolu de votre projet

DATA_PATH = ROOT / "data" / "processed" / "panel_estimation.csv"
OUT_CSV   = ROOT / "results" / "table3_robustness.csv"
OUT_FIG   = ROOT / "results" / "figures" / "figure3_placebo.png"
CKPT_CSV  = ROOT / "results" / "table3_placebo_checkpoint.csv"

OUT_FIG.parent.mkdir(parents=True, exist_ok=True)

# Coefficients de reference (modele V3 baseline spec b -- S_plus remplace spei_mean)
BETA1_BASELINE = 0.00545
SE1_BASELINE   = 0.00271

# =============================================================================
# 1. CHARGEMENT DES DONNEES
# =============================================================================
print("=" * 65)
print("CHARGEMENT DES DONNEES")
print("=" * 65)
sys.stdout.flush()

df = pd.read_csv(DATA_PATH)

print(f"  Observations       : {len(df):,}")
print(f"  Regions            : {df['region_id'].nunique():,}")
print(f"  Annees             : {df['year'].min()} - {df['year'].max()}")
sys.stdout.flush()

required_cols = ["region_id", "year", "growth", "D_v3",
                 "S_plus", "T_it", "t_var", "T_bar_temp"]
missing = [c for c in required_cols if c not in df.columns]
if missing:
    raise ValueError(f"Colonnes manquantes dans le panel : {missing}")

df = df.dropna(subset=required_cols).copy()
print(f"  Apres dropna       : {len(df):,} observations")
sys.stdout.flush()

df["D_v3_x_Tbar"] = df["D_v3"] * df["T_bar_temp"]
df["year_c"]      = df["year"] - df["year"].mean()

print()

# =============================================================================
# 2. UTILITAIRES
# =============================================================================
REGRESSORS = ["D_v3", "D_v3_x_Tbar", "S_plus", "T_it", "t_var"]


def estimer_v3(data, dep_col="growth", reg_cols=None):
    """Estime le modele V3 (PanelOLS, entity + time effects, cluster region)."""
    if reg_cols is None:
        reg_cols = REGRESSORS
    pdata = data.set_index(["region_id", "year"])
    mod = PanelOLS(
        dependent      = pdata[dep_col],
        exog           = pdata[reg_cols],
        entity_effects = True,
        time_effects   = True,
        drop_absorbed  = True,
    )
    res   = mod.fit(cov_type="clustered", cluster_entity=True)
    beta1 = res.params[reg_cols[0]]
    se1   = res.std_errors[reg_cols[0]]
    return beta1, se1, res


def detrend_fw(data, variables):
    """
    Frisch-Waugh : retire les tendances lineaires region-specifiques.
    Retourne un DataFrame avec colonnes suffixees '_dt'.
    """
    df_out = data.copy()
    result_cols = {v: np.full(len(data), np.nan) for v in variables}

    for rid, grp in data.groupby("region_id"):
        idx = grp.index
        n   = len(grp)
        if n < 4:
            continue
        X = np.column_stack([np.ones(n), grp["year_c"].values])
        try:
            XtXinv = np.linalg.inv(X.T @ X)
        except np.linalg.LinAlgError:
            continue
        M = np.eye(n) - X @ XtXinv @ X.T
        for v in variables:
            result_cols[v][data.index.get_indexer(idx)] = M @ grp[v].values

    for v in variables:
        df_out[f"{v}_dt"] = result_cols[v]

    return df_out


# =============================================================================
# 3. TEST 1 -- TENDANCES LINEAIRES REGION-SPECIFIQUES (FRISCH-WAUGH)
# =============================================================================
print("=" * 65)
print("TEST 1 -- TENDANCES LINEAIRES REGION-SPECIFIQUES")
print("=" * 65)
sys.stdout.flush()

vars_to_detrend = ["growth", "D_v3", "D_v3_x_Tbar", "S_plus", "T_it", "t_var"]

print("  Application de Frisch-Waugh par region...")
sys.stdout.flush()
df_dt = detrend_fw(df, vars_to_detrend)

dt_cols = [f"{v}_dt" for v in vars_to_detrend]
df_dt   = df_dt.dropna(subset=dt_cols).copy()
print(f"  Observations apres detrending : {len(df_dt):,}")
sys.stdout.flush()

reg_cols_dt = ["D_v3_dt", "D_v3_x_Tbar_dt", "S_plus_dt", "T_it_dt", "t_var_dt"]

beta1_t1, se1_t1, res_t1 = estimer_v3(
    df_dt, dep_col="growth_dt", reg_cols=reg_cols_dt
)

variation_t1 = (beta1_t1 - BETA1_BASELINE) / abs(BETA1_BASELINE) * 100

print()
print(f"  beta1 baseline           : {BETA1_BASELINE:.5f}  (SE = {SE1_BASELINE:.5f})")
print(f"  beta1 avec tendances reg.: {beta1_t1:.5f}  (SE = {se1_t1:.5f})")
print(f"  Variation vs baseline    : {variation_t1:+.2f}%")
print()
sys.stdout.flush()

# =============================================================================
# 4. TEST 2 -- TEST PLACEBO PAR PERMUTATION DES SERIES TEMPORELLES
# =============================================================================
print("=" * 65)
print("TEST 2 -- TEST PLACEBO PAR PERMUTATION (N_PERM iterations)")
print("=" * 65)
sys.stdout.flush()

N_PERM = 500
SEED   = 42
rng    = np.random.default_rng(SEED)

df_sorted   = df.sort_values(["region_id", "year"]).reset_index(drop=True)
regions_arr = df_sorted["region_id"].unique()
N_regions   = len(regions_arr)

D_orig   = df_sorted["D_v3"].values.copy()
T_bar_np = df_sorted["T_bar_temp"].values

region_pos_np = {}
for rid in regions_arr:
    region_pos_np[rid] = np.where(df_sorted["region_id"].values == rid)[0]

region_D_np = {rid: D_orig[region_pos_np[rid]] for rid in regions_arr}

betas_placebo = np.full(N_PERM, np.nan)
n_skip        = 0
t0            = time.time()

print(f"  Demarrage -- {N_PERM} permutations, seed={SEED}")
print()
sys.stdout.flush()

for b in range(N_PERM):

    if (b + 1) % 100 == 0:
        elapsed  = time.time() - t0
        per_iter = elapsed / (b + 1)
        rem_min  = per_iter * (N_PERM - b - 1) / 60
        print(
            f"  iter {b+1:4d}/{N_PERM}  "
            f"{elapsed/60:.1f}min ecoule  "
            f"~{rem_min:.1f}min restant  "
            f"skip={n_skip}",
            flush=True,
        )
        _ok = betas_placebo[:b + 1][~np.isnan(betas_placebo[:b + 1])]
        if len(_ok) >= 5:
            _pct = float(np.mean(_ok <= BETA1_BASELINE) * 100)
            _pv  = float(np.mean(np.abs(_ok) >= abs(BETA1_BASELINE)))
            pd.DataFrame([{
                "iter": b + 1, "n_valid": len(_ok), "n_skip": n_skip,
                "placebo_mean": float(_ok.mean()), "placebo_std": float(_ok.std()),
                "percentile": _pct, "p_value_emp": _pv,
            }]).to_csv(CKPT_CSV, index=False, encoding="utf-8")

    perm   = rng.permutation(N_regions)
    donors = regions_arr[perm]

    D_b = D_orig.copy()
    for k in range(N_regions):
        rid   = regions_arr[k]
        donor = donors[k]
        pos_r = region_pos_np[rid]
        D_d   = region_D_np[donor]
        n_min = min(len(pos_r), len(D_d))
        D_b[pos_r[:n_min]] = D_d[:n_min]
        if len(pos_r) > n_min:
            D_b[pos_r[n_min:]] = np.nan

    DxT_b    = D_b * T_bar_np
    valid_np = (
        ~np.isnan(D_b) &
        ~np.isnan(DxT_b) &
        ~np.isnan(df_sorted["growth"].values) &
        ~np.isnan(df_sorted["S_plus"].values) &
        ~np.isnan(df_sorted["T_it"].values) &
        ~np.isnan(df_sorted["t_var"].values)
    )

    if valid_np.sum() < 1000:
        n_skip += 1
        continue

    df_b = df_sorted.loc[valid_np, ["region_id", "year", "growth",
                                     "S_plus", "T_it", "t_var", "T_bar_temp"]].copy()
    df_b["D_v3"]        = D_b[valid_np]
    df_b["D_v3_x_Tbar"] = DxT_b[valid_np]

    try:
        beta_b, _, _ = estimer_v3(df_b)
        betas_placebo[b] = beta_b
    except Exception:
        n_skip += 1

    if (b + 1) % 50 == 0:
        gc.collect()

betas_valid = betas_placebo[~np.isnan(betas_placebo)]
n_valid     = len(betas_valid)

placebo_mean = float(betas_valid.mean())
placebo_std  = float(betas_valid.std())
percentile   = float(np.mean(betas_valid <= BETA1_BASELINE) * 100)
p_value_emp  = float(np.mean(np.abs(betas_valid) >= abs(BETA1_BASELINE)))
p99_placebo  = float(np.percentile(betas_valid, 1))

print()
print(f"  Iterations valides          : {n_valid}/{N_PERM}")
if n_skip > N_PERM * 0.02:
    print(f"  ATTENTION : {n_skip / N_PERM * 100:.1f}% iterations skippees > 2%")
print(f"  Moyenne beta1 placebo       : {placebo_mean:.6f}")
print(f"  Ecart-type beta1 placebo    : {placebo_std:.6f}")
print(f"  Percentile beta1 reel       : {percentile:.2f}e percentile")
print(f"  p-value empirique           : {p_value_emp:.4f}")
print(f"  p99 (1er percentile)        : {p99_placebo:.6f}")
print()
sys.stdout.flush()

# =============================================================================
# 5. EXPORT CSV
# =============================================================================
rows = [
    {"test": "baseline",
     "beta1": BETA1_BASELINE,
     "se": SE1_BASELINE,
     "note": "Modele V3 baseline (101_baseline_regression.py)"},
    {"test": "region_trends",
     "beta1": round(beta1_t1, 6),
     "se": round(se1_t1, 6),
     "note": f"V3 + tendances lineaires region-specifiques (Frisch-Waugh) delta={variation_t1:+.2f}%"},
    {"test": "placebo_mean",
     "beta1": round(placebo_mean, 6),
     "se": round(placebo_std, 6),
     "note": f"Moyenne beta1 placebo (N={n_valid}, p={p_value_emp:.4f})"},
    {"test": "placebo_p99",
     "beta1": round(p99_placebo, 6),
     "se": float("nan"),
     "note": "1er percentile distribution placebo (queue gauche)"},
]

df_out = pd.DataFrame(rows, columns=["test", "beta1", "se", "note"])
df_out.to_csv(OUT_CSV, index=False, encoding="utf-8")
print(f"  CSV sauvegarde : {OUT_CSV}")
print(df_out.to_string(index=False))
sys.stdout.flush()

# =============================================================================
# 6. FIGURE -- HISTOGRAMME PLACEBO
# =============================================================================
fig, ax = plt.subplots(figsize=(9, 5))

ax.hist(betas_valid, bins=40, color="#4878CF", edgecolor="white",
        linewidth=0.4, alpha=0.85,
        label=f"Coefficients placebo (n={n_valid})")
ax.axvline(BETA1_BASELINE, color="#D62728", linewidth=2.0, linestyle="--",
           label=f"beta1 reel = {BETA1_BASELINE:.4f}")
ax.axvline(0, color="black", linewidth=0.8, linestyle=":", alpha=0.5)

ylim_top = ax.get_ylim()[1] if ax.get_ylim()[1] > 0 else 10
ax.text(BETA1_BASELINE * 1.05, ylim_top * 0.5,
        f"p-value = {p_value_emp:.3f}\nPctile  = {percentile:.1f}e",
        fontsize=9, color="#D62728",
        ha="right" if BETA1_BASELINE < 0 else "left")

ax.set_xlabel("beta1 (coefficient sur D_v3)", fontsize=11)
ax.set_ylabel("Frequence", fontsize=11)
ax.set_title("Test placebo -- distribution des coefficients sous permutation",
             fontsize=12, pad=10)
ax.legend(fontsize=10)
ax.xaxis.set_major_formatter(mticker.FormatStrFormatter("%.4f"))
plt.tight_layout()
fig.savefig(OUT_FIG, dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Figure sauvegardee : {OUT_FIG}")
sys.stdout.flush()

# =============================================================================
# 7. SYNTHESE CONSOLE
# =============================================================================
print()
print("=" * 65)
print("SYNTHESE DES TESTS DE ROBUSTESSE")
print("=" * 65)
print(f"  Baseline V3    beta1 = {BETA1_BASELINE:.5f}  SE = {SE1_BASELINE:.5f}")
print(f"  Tendances reg. beta1 = {beta1_t1:.5f}  SE = {se1_t1:.5f}"
      f"  (delta = {variation_t1:+.2f}%)")
print(f"  Placebo moy.   beta1 = {placebo_mean:.5f}  sigma = {placebo_std:.5f}"
      f"  p = {p_value_emp:.4f}")
print()

if abs(variation_t1) <= 20:
    print("  [TEST 1] Stable : variation <= 20% par rapport au baseline.")
else:
    print("  [TEST 1] ATTENTION : variation > 20% -- les tendances regionales"
          " absorbent une part notable de l'effet estime.")

if p_value_emp <= 0.05:
    print("  [TEST 2] OK : beta1 reel distinguable de H0 (p <= 0.05).")
else:
    print("  [TEST 2] ATTENTION : p > 0.05 -- beta1 reel non distinguable"
          " de la distribution sous permutation.")

print("=" * 65)
sys.stdout.flush()

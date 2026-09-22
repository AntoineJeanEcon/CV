"""
106_placebo_marginal.py
=======================
Placebo re-cible sur l'AME (quantite d'interet reelle du modele V3).

  Bloc B1 -- Placebo AME
    Permutation de D_v3 entre regions -> re-estime V3 complet (avec interaction)
    -> AME_perm = beta1_perm + beta2_perm * T_bar_moyen
    Compare le vrai AME (-0.00954 pp) a la distribution des AME permutes.

  Bloc B2 -- Diagnostic non-centrage
    Meme permutation, spec SANS interaction (D_v3 seul + controles)
    -> teste si la moyenne non-centree (+0.00227 dans 06_robustness) vient de
       l'interaction.

Sortie    : results/table_placebo_ame.csv
Checkpoint: results/placebo_ame_checkpoint.csv
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
OUT_CSV   = ROOT / "results" / "table_placebo_ame.csv"
CKPT_CSV  = ROOT / "results" / "placebo_ame_checkpoint.csv"
OUT_FIG   = ROOT / "results" / "figures" / "figure_placebo_ame.png"
OUT_FIG.parent.mkdir(parents=True, exist_ok=True)

# Valeurs de reference (baseline spec b)
AME_BASELINE   = -0.009540   # beta1 + beta2 * T_bar
BETA1_BASELINE =  0.00545    # coef D_v3 seul (pour comparaison bloc B2)

N_PERM = 500
SEED   = 42

# =============================================================================
# 1. CHARGEMENT
# =============================================================================
print("=" * 65)
print("CHARGEMENT DES DONNEES")
print("=" * 65)
sys.stdout.flush()

df = pd.read_csv(DATA_PATH)
required = ["region_id", "year", "growth", "D_v3", "S_plus", "T_it", "t_var", "T_bar_temp"]
df = df.dropna(subset=required).copy()
df["D_v3_x_Tbar"] = df["D_v3"] * df["T_bar_temp"]

print(f"  Observations : {len(df):,}  |  Regions : {df['region_id'].nunique():,}")

T_bar_mean = df["T_bar_temp"].mean()
print(f"  T_bar moyen panel : {T_bar_mean:.4f} C")
sys.stdout.flush()

# =============================================================================
# 2. PRE-CALCUL NUMPY (commun aux deux blocs)
# =============================================================================
df_sorted   = df.sort_values(["region_id", "year"]).reset_index(drop=True)
regions_arr = df_sorted["region_id"].unique()
N_regions   = len(regions_arr)

D_orig   = df_sorted["D_v3"].values.copy()
T_bar_np = df_sorted["T_bar_temp"].values

region_pos_np = {rid: np.where(df_sorted["region_id"].values == rid)[0]
                 for rid in regions_arr}
region_D_np   = {rid: D_orig[region_pos_np[rid]] for rid in regions_arr}

rng = np.random.default_rng(SEED)

REGS_FULL  = ["D_v3", "D_v3_x_Tbar", "S_plus", "T_it", "t_var"]
REGS_NOINT = ["D_v3", "S_plus", "T_it", "t_var"]


def permute_D(rng_):
    """Retourne un vecteur D_v3 permute et le DxT associe."""
    perm   = rng_.permutation(N_regions)
    donors = regions_arr[perm]
    D_b    = D_orig.copy()
    for k in range(N_regions):
        pos_r = region_pos_np[regions_arr[k]]
        D_d   = region_D_np[donors[k]]
        n_min = min(len(pos_r), len(D_d))
        D_b[pos_r[:n_min]] = D_d[:n_min]
        if len(pos_r) > n_min:
            D_b[pos_r[n_min:]] = np.nan
    DxT_b = D_b * T_bar_np
    return D_b, DxT_b


def estimer(df_b, reg_cols):
    """PanelOLS entity+time effects, cluster region. Retourne params dict."""
    pdata = df_b.set_index(["region_id", "year"])
    mod = PanelOLS(
        dependent      = pdata["growth"],
        exog           = pdata[reg_cols],
        entity_effects = True,
        time_effects   = True,
        drop_absorbed  = True,
    )
    res = mod.fit(cov_type="clustered", cluster_entity=True)
    return {v: res.params[v] for v in reg_cols}


# =============================================================================
# 3. BLOC B1 -- PLACEBO SUR L'AME
# =============================================================================
print()
print("=" * 65)
print("BLOC B1 -- PLACEBO AME (V3 complet avec interaction)")
print(f"  N_PERM={N_PERM}, seed={SEED}")
print("=" * 65)
sys.stdout.flush()

ame_placebo = np.full(N_PERM, np.nan)
b1_b1_arr   = np.full(N_PERM, np.nan)
b2_b1_arr   = np.full(N_PERM, np.nan)
n_skip_b1   = 0
t0          = time.time()

for b in range(N_PERM):

    if (b + 1) % 100 == 0:
        elapsed  = time.time() - t0
        per_iter = elapsed / (b + 1)
        rem_min  = per_iter * (N_PERM - b - 1) / 60
        print(f"  B1 iter {b+1:4d}/{N_PERM}  {elapsed/60:.1f}min ecoule  "
              f"~{rem_min:.1f}min restant  skip={n_skip_b1}", flush=True)
        _ok = ame_placebo[:b+1][~np.isnan(ame_placebo[:b+1])]
        if len(_ok) >= 5:
            pd.DataFrame([{
                "bloc": "B1", "iter": b+1, "n_valid": len(_ok),
                "n_skip": n_skip_b1,
                "ame_mean": float(_ok.mean()), "ame_std": float(_ok.std()),
                "percentile_true": float(np.mean(_ok <= AME_BASELINE) * 100),
                "p_value_left": float(np.mean(_ok <= AME_BASELINE)),
            }]).to_csv(CKPT_CSV, index=False, encoding="utf-8")

    D_b, DxT_b = permute_D(rng)

    valid_np = (
        ~np.isnan(D_b) & ~np.isnan(DxT_b) &
        ~np.isnan(df_sorted["growth"].values) &
        ~np.isnan(df_sorted["S_plus"].values) &
        ~np.isnan(df_sorted["T_it"].values) &
        ~np.isnan(df_sorted["t_var"].values)
    )
    if valid_np.sum() < 1000:
        n_skip_b1 += 1
        continue

    df_b = df_sorted.loc[valid_np, ["region_id", "year", "growth",
                                     "S_plus", "T_it", "t_var"]].copy()
    df_b["D_v3"]        = D_b[valid_np]
    df_b["D_v3_x_Tbar"] = DxT_b[valid_np]

    try:
        params         = estimer(df_b, REGS_FULL)
        b1_b1_arr[b]   = params["D_v3"]
        b2_b1_arr[b]   = params["D_v3_x_Tbar"]
        ame_placebo[b] = params["D_v3"] + params["D_v3_x_Tbar"] * T_bar_mean
    except Exception:
        n_skip_b1 += 1

    if (b + 1) % 50 == 0:
        gc.collect()

ame_valid  = ame_placebo[~np.isnan(ame_placebo)]
n_valid_b1 = len(ame_valid)

ame_mean = float(ame_valid.mean())
ame_std  = float(ame_valid.std())
pct_true = float(np.mean(ame_valid <= AME_BASELINE) * 100)
p_left   = float(np.mean(ame_valid <= AME_BASELINE))

print()
print(f"  Iterations valides B1 : {n_valid_b1}/{N_PERM} (skip={n_skip_b1})")
if n_skip_b1 > N_PERM * 0.02:
    print(f"  ATTENTION : {n_skip_b1/N_PERM*100:.1f}% skips > 2%")
print(f"  AME placebo -- moyenne : {ame_mean:+.5f}  SD : {ame_std:.5f}")
print(f"  Vrai AME                : {AME_BASELINE:+.5f} pp")
print(f"  Percentile vrai AME     : {pct_true:.1f}e percentile")
print(f"  p-value unilateral gche : {p_left:.4f}")
print()
sys.stdout.flush()

# =============================================================================
# 4. BLOC B2 -- DIAGNOSTIC SPEC SANS INTERACTION
# =============================================================================
print("=" * 65)
print("BLOC B2 -- DIAGNOSTIC (V3 SANS interaction)")
print(f"  N_PERM={N_PERM}, meme seed (continuation)")
print("=" * 65)
sys.stdout.flush()

rng2      = np.random.default_rng(SEED)
b1_noint  = np.full(N_PERM, np.nan)
n_skip_b2 = 0
t0b2      = time.time()

for b in range(N_PERM):

    if (b + 1) % 100 == 0:
        elapsed  = time.time() - t0b2
        per_iter = elapsed / (b + 1)
        rem_min  = per_iter * (N_PERM - b - 1) / 60
        print(f"  B2 iter {b+1:4d}/{N_PERM}  {elapsed/60:.1f}min ecoule  "
              f"~{rem_min:.1f}min restant  skip={n_skip_b2}", flush=True)

    D_b, _ = permute_D(rng2)

    valid_np = (
        ~np.isnan(D_b) &
        ~np.isnan(df_sorted["growth"].values) &
        ~np.isnan(df_sorted["S_plus"].values) &
        ~np.isnan(df_sorted["T_it"].values) &
        ~np.isnan(df_sorted["t_var"].values)
    )
    if valid_np.sum() < 1000:
        n_skip_b2 += 1
        continue

    df_b = df_sorted.loc[valid_np, ["region_id", "year", "growth",
                                     "S_plus", "T_it", "t_var"]].copy()
    df_b["D_v3"] = D_b[valid_np]

    try:
        params      = estimer(df_b, REGS_NOINT)
        b1_noint[b] = params["D_v3"]
    except Exception:
        n_skip_b2 += 1

    if (b + 1) % 50 == 0:
        gc.collect()

b1_noint_valid = b1_noint[~np.isnan(b1_noint)]
n_valid_b2     = len(b1_noint_valid)
b1_noint_mean  = float(b1_noint_valid.mean())
b1_noint_std   = float(b1_noint_valid.std())

print()
print(f"  Iterations valides B2  : {n_valid_b2}/{N_PERM} (skip={n_skip_b2})")
print(f"  beta1 placebo (sans interaction) -- moyenne : {b1_noint_mean:+.6f}")
print(f"  beta1 placebo (avec interaction) -- moyenne : +0.002273  (issu de 06_robustness)")
if abs(b1_noint_mean) < abs(0.002273) * 0.5:
    print("  -> Recentrage confirme : l'interaction explique le non-centrage.")
else:
    print("  -> Recentrage partiel ou absent.")
sys.stdout.flush()

# =============================================================================
# 5. EXPORT CSV
# =============================================================================
rows = [
    {"bloc": "B1_ame_true",         "value": AME_BASELINE,
     "note": "Vrai AME baseline (beta1 + beta2*T_bar_mean)"},
    {"bloc": "B1_ame_placebo_mean", "value": ame_mean,
     "note": f"Moyenne AME placebo (N={n_valid_b1})"},
    {"bloc": "B1_ame_placebo_std",  "value": ame_std,
     "note": "Ecart-type AME placebo"},
    {"bloc": "B1_percentile_true",  "value": pct_true,
     "note": "Percentile du vrai AME dans distribution placebo"},
    {"bloc": "B1_pvalue_left",      "value": p_left,
     "note": "p-value unilateral gauche (part AME_perm <= vrai AME)"},
    {"bloc": "B2_b1_noint_mean",    "value": b1_noint_mean,
     "note": f"Moyenne beta1 placebo sans interaction (N={n_valid_b2})"},
    {"bloc": "B2_b1_noint_std",     "value": b1_noint_std,
     "note": "SD beta1 placebo sans interaction"},
]

pd.DataFrame(rows).to_csv(OUT_CSV, index=False, encoding="utf-8")
print(f"==> CSV : {OUT_CSV}", flush=True)

# =============================================================================
# 6. FIGURE
# =============================================================================
fig, axes = plt.subplots(1, 2, figsize=(13, 5))

ax = axes[0]
ax.hist(ame_valid, bins=40, color="#4878CF", edgecolor="white",
        linewidth=0.4, alpha=0.85, label=f"AME placebo (n={n_valid_b1})")
ax.axvline(AME_BASELINE, color="#D62728", linewidth=2.0, linestyle="--",
           label=f"Vrai AME = {AME_BASELINE:.4f}")
ax.axvline(0, color="black", linewidth=0.8, linestyle=":", alpha=0.5)
ylim = ax.get_ylim()[1] if ax.get_ylim()[1] > 0 else 10
ax.text(AME_BASELINE * 0.95, ylim * 0.55,
        f"p = {p_left:.3f}\nPctile = {pct_true:.1f}e",
        fontsize=9, color="#D62728", ha="right")
ax.set_xlabel("AME placebo (pp)", fontsize=10)
ax.set_ylabel("Frequence", fontsize=10)
ax.set_title("B1 -- Placebo sur AME", fontsize=11)
ax.legend(fontsize=9)
ax.xaxis.set_major_formatter(mticker.FormatStrFormatter("%.4f"))

ax = axes[1]
ax.hist(b1_noint_valid, bins=40, color="#5DAB56", edgecolor="white",
        linewidth=0.4, alpha=0.85,
        label=f"beta1 placebo sans inter. (n={n_valid_b2})")
ax.axvline(BETA1_BASELINE, color="#D62728", linewidth=2.0, linestyle="--",
           label=f"Vrai beta1 = {BETA1_BASELINE:.4f}")
ax.axvline(0, color="black", linewidth=0.8, linestyle=":", alpha=0.5)
ax.set_xlabel("beta1 placebo (sans interaction)", fontsize=10)
ax.set_ylabel("Frequence", fontsize=10)
ax.set_title("B2 -- Diagnostic non-centrage (sans interaction)", fontsize=11)
ax.legend(fontsize=9)
ax.xaxis.set_major_formatter(mticker.FormatStrFormatter("%.4f"))

plt.suptitle("Tests placebo -- quantite d'interet et diagnostic", fontsize=12)
plt.tight_layout()
fig.savefig(OUT_FIG, dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"==> Figure : {OUT_FIG}", flush=True)

# =============================================================================
# 7. SYNTHESE
# =============================================================================
print()
print("=" * 65)
print("SYNTHESE")
print("=" * 65)
print(f"  [B1] Vrai AME       : {AME_BASELINE:+.5f} pp")
print(f"  [B1] AME placebo    : {ame_mean:+.5f} pp (SD={ame_std:.5f})")
print(f"  [B1] Percentile     : {pct_true:.1f}e  |  p unilateral = {p_left:.4f}")
print(f"  [B2] beta1 (no int) : {b1_noint_mean:+.6f} (vs +0.002273 avec interaction)")
if abs(b1_noint_mean) < 0.001:
    print("  [B2] -> Distribution recentree pres de 0 : interaction explique le biais.")
else:
    print("  [B2] -> Recentrage partiel.")
print("=" * 65)
sys.stdout.flush()

# Chocs de sécheresse et croissance économique - replication code

Code utilisé pour l'analyse empirique du mémoire de M1 *« Chocs hydriques et croissance économique : une transposition de Callahan & Mankin (2022) sur un panel régional mondial »*. Le pipeline estime l'effet de l'intensité des déficits hydriques (SPEI-12) sur la croissance du PIB par habitant sous-national à l'aide d'un modèle de panel à doubles effets fixes, puis attribue une part de cet effet au forçage anthropique via les simulations CMIP6 DAMIP.

---

## Pipeline d'exécution

Le pipeline est organisé en **deux branches indépendantes** (série 000 et série 100) qui convergent dans le script d'attribution finale (série 200).

```
BRANCHE 000                        BRANCHE 100                        SÉRIE 200
Préparation climatique CMIP6       Estimation économétrique           Convergence
════════════════════════           ══════════════════════             ═══════════
000_download_cmip6.py              100_build_panel.py
        │                                  │
001_compute_spei_cmip6.py          101_baseline_regression.py
        │                                  │
002_compute_delta_spei.py          102_distributed_lag.py
        │                                  │
        │   Δ SPEI                         │  coeff. DLM
        └──────────────────────────────────┴────────────────────────► 200_attribution.py
                                           │
                                   103_conley_se.py
                                           │
                                   104_damages.py
                                           │
                                   105_robustness.py
                                           │
                                   106_placebo_marginal.py
                                           │
                                   107_collinearity_vif.py
```

La branche 000 peut être exécutée en parallèle de la branche 100. Le script `200_attribution.py` requiert à la fois les coefficients DLM produits par `102_distributed_lag.py` et les deltas SPEI CMIP6 produits par `002_compute_delta_spei.py`. Les scripts 103–107 sont indépendants de 200 et s'exécutent après 102.

---

## Scripts — Branche 000 : préparation climatique CMIP6

| Script | Description |
|--------|-------------|
| `000_download_cmip6.py` | Téléchargement des sorties CMIP6 (pr, tas) via l'API ESGF pour les expériences `historical` et `hist-nat`, 10 modèles DAMIP |
| `001_compute_spei_cmip6.py` | Calcul du SPEI-12 sur chaque modèle CMIP6 (ETP Hargreaves-Samani, DTR=12°C fixe, ajustement log-logistique par L-moments) |
| `002_compute_delta_spei.py` | Delta anthropogénique SPEI-12 = hist − hist-nat, minimum annuel, regrid sur grille 2.5° commune, moyenne d'ensemble multi-modèle |

## Scripts — Branche 100 : estimation économétrique

| Script | Description |
|--------|-------------|
| `100_build_panel.py` | Construction du panel sous-national (2 832 régions GADM-1, 1992–2013) : merge PIB × climat, variables dérivées (D_v3, S_plus, lags 1–4), waterfall de filtrage selon les règles de Callahan & Mankin (2022) |
| `101_baseline_regression.py` | Estimation de la régression baseline (équation 4.1) : trois spécifications V1/V2/V3, TWFE (effets fixes région + année), erreurs clusterisées par région |
| `102_distributed_lag.py` | Estimation du modèle à lags distribués (lags 0–4 de D_v3), IC 95% par méthode delta et bootstrap par bloc (N=1000, seed=42) |
| `103_conley_se.py` | Erreurs standard de Conley (1999), noyau Bartlett spatial (500/1000/2000 km) et temporel (L=3 lags), implémentation vectorisée année par année |
| `104_damages.py` | Calcul des pertes économiques : Δg_it × gdp_obs_it (sans compounding), agrégation par région et continent, IC 90% bootstrap paramétrique |
| `105_robustness.py` | Tests de robustesse : tendances linéaires région-spécifiques (Frisch-Waugh) + test placebo par permutation des séries (N=500) |
| `106_placebo_marginal.py` | Placebo re-ciblé sur l'AME (β1 + β2 × T̄) : bloc B1 (distribution AME sous permutation, p-value unilatéral) + bloc B2 (diagnostic non-centrage sans interaction) |
| `107_collinearity_vif.py` | Diagnostic de collinéarité : corrélation de Pearson D_v3/T̄, VIF après within-transformation région×année, notes méthodologiques sur T̄ time-invariant |

## Script de convergence — Série 200

| Script | Description |
|--------|-------------|
| `200_attribution.py` | Attribution économique : delta SPEI CMIP6 → ΔD_it = −min(Δ SPEI, 0), effet attribué cumulé 5 lags (Σ β̂_k × ΔD_{i,t-k}), Monte Carlo croisé β × modèle CMIP6 (N=1000) |

---

## Dépendances Python

```
linearmodels>=4.30    # TWFE, PanelOLS
pandas>=1.5
numpy>=1.23
xarray>=2022.11
netCDF4>=1.6
geopandas>=0.12
scipy>=1.9
statsmodels>=0.13
matplotlib>=3.6
pyet                  # ETP (Hargreaves-Samani)
pycountry             # mapping ISO-3 → continent (optionnel)
pycountry-convert     # idem (optionnel)
```

Installation :
```bash
conda create -n memoire python=3.10
conda activate memoire
pip install linearmodels pandas numpy xarray netCDF4 geopandas scipy statsmodels matplotlib pyet pycountry pycountry-convert
```

---

## Configuration des chemins

Chaque script commence par :
```python
ROOT = Path(".")  # remplacez par le chemin absolu de votre projet
```

L'arborescence attendue sous `ROOT` est :
```
ROOT/
├── data/
│   ├── raw/
│   │   ├── gdp.csv                          # PIB sous-national (C&M / K&W)
│   │   ├── gadm36_levels_shp/               # Shapefiles GADM niveau 1
│   │   └── population/                      # GPW interpolé par région
│   ├── processed/                           # Fichiers intermédiaires générés
│   └── cmip6/
│       ├── raw/                             # Sorties CMIP6 brutes (ESGF)
│       └── processed/                       # SPEI-12 et deltas calculés
└── results/                                 # Tables et figures générées
```

---

## Données externes

Les données ne sont **pas** incluses dans ce dépôt. Elles doivent être téléchargées séparément.

### 1. PIB sous-national (Callahan & Mankin 2022 / Kalkuhl & Wenz 2020)
- **Source** : Callahan, C.W. & Mankin, J.S. (2022), *Science Advances*, 8(43)
- **Accès** : dépôt public des auteurs (voir supplément de l'article)
- **Couverture** : ~2 800 régions GADM-1, 1979–2014, PIB/hab en USD 2011

### 2. SPEIbase v2.10 (CSIC)
- **Source** : Beguería, S. et al., CSIC (Consejo Superior de Investigaciones Científicas)
- **Accès** : https://spei.csic.es/database.html
- **Variable** : SPEI sur 12 mois (SPEI-12), résolution 0.5°, 1901–2020

### 3. Réanalyse ERA5 (ECMWF)
- **Source** : Hersbach et al. (2020), *QJRMS*
- **Accès** : https://cds.climate.copernicus.eu (Copernicus Climate Data Store)
- **Variables** : `2m_temperature` (tas), `total_precipitation` (pr)
- **Résolution** : 0.25°, mensuelle

### 4. Simulations CMIP6 DAMIP (ESGF)
- **Source** : Gillett et al. (2016), *Geoscientific Model Development*
- **Accès** : https://esgf-node.llnl.gov/search/cmip6/
- **Expériences** : `historical` + `hist-nat`
- **Variables** : `pr` (précipitations), `tas` (température de surface)
- **Modèles** : MIROC6, CanESM5, IPSL-CM6A-LR, CNRM-CM6-1, HadGEM3-GC31-LL, MRI-ESM2-0, BCC-CSM2-MR, GFDL-ESM4, ACCESS-ESM1-5, NorESM2-LM

### 5. Population GPW v4 (SEDAC)
- **Source** : CIESIN (2018), Gridded Population of the World v4
- **Accès** : https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11
- **Usage** : interpolation annuelle au niveau GADM-1 pour la valorisation monétaire (`104_damages.py`)

---

## Référence

Callahan, C.W. & Mankin, J.S. (2022). Globally unequal effect of extreme heat on economic growth. *Science Advances*, 8(43), eadd3726. https://doi.org/10.1126/sciadv.add3726

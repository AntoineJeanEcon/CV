# Projet M1 Économie — Détection de bulle spéculative sur le Bitcoin

**Auteur :** [Antoine Jean]
**Encadrant :** Pr Jamal Bouoiyour
**Année universitaire :** 2025–2026

## Objet

Ce dépôt contient le code R et le rapport LaTeX du projet de fin de semestre visant à tester empiriquement la présence de bulles spéculatives sur le Bitcoin (BTC/USD) sur la période 2015–2024.

La méthodologie combine :
- Analyse descriptive (moments, normalité, queues de distribution)
- Tests de stationnarité (ADF, KPSS, Phillips-Perron)
- Tests de cointégration (Phillips-Ouliaris, Johansen)
- Modèle VAR en différences, causalité de Granger, IRF, FEVD
- Tests de racine unitaire explosive SADF et GSADF (Phillips, Shi & Yu, 2015)

## Données

Source : **Yahoo Finance**, ticker `BTC-USD`, fréquence journalière, période 01/01/2015 → 31/12/2024 (3 653 observations).

Les données ne sont **pas incluses dans le dépôt** : elles sont téléchargées automatiquement à l'exécution du script `01-btc_analyse_descriptive.R` via le package `quantmod` (fonction `getSymbols`). Aucune clé API n'est requise.

**Avertissement** : l'API Yahoo peut occasionnellement modifier son format ou présenter des indisponibilités. Si le téléchargement échoue, ré-essayer après quelques minutes ou changer de réseau.

## Prérequis

- **R** version 4.5 ou supérieure
- Packages R requis (à installer une seule fois) :

```r
install.packages(c(
    "quantmod", "ggplot2", "moments", "tseries", "xts", "zoo",
    "knitr", "kableExtra", "scales", "patchwork", "lubridate",
    "urca", "vars", "tsDyn", "MultipleBubbles", "dplyr"
))
```

## Structure du dépôt

```
.
├── README.md
├── 01-btc_analyse_descriptive.R       # Script #1 : téléchargement, descriptive
├── 02-stationnarite_cointegration_VAR.R # Script #2 : stationnarité, cointégration, VAR
├── 03-Tests_SADF_GSADF.R              # Script #3 : SADF, GSADF, date-stamping
└── rapport_btc_bulle.tex              # Rapport LaTeX
```

## Ordre d'exécution

Les scripts doivent être exécutés **dans l'ordre** dans la même session R, car chacun utilise des objets créés par les précédents :

1. `01-btc_analyse_descriptive.R` : télécharge les données, calcule les statistiques descriptives, produit les graphiques `btc_*.png`.
   Objets créés : `df`, `df_ret`, `prix`.

2. `02-stationnarite_cointegration_VAR.R` : tests de stationnarité, de cointégration, estimation du VAR, tests de Granger, IRF et FEVD.
   Objets créés : `prix_clean`, `modele_var`, `tab_stat`, `tab_jo`.
   Graphiques produits : `IRF_*.png`.

3. `03-Tests_SADF_GSADF.R` : tests SADF et GSADF, date-stamping des épisodes explosifs.
   Graphiques produits : `bitcoin_logprix_episodes_explosifs.png` et `bitcoin_bsadf_sequence.png`.

## Sorties générées

À la fin de l'exécution, le répertoire de travail contient :

- **Graphiques PNG** (300 dpi) :
  `btc_prix_niveau.png`, `btc_log_rendements.png`,
  `btc_histogramme_ret.png`, `btc_qqplot_ret.png`,
  `IRF_prix_vers_volume.png`, `IRF_volume_vers_prix.png`,
  `bitcoin_logprix_episodes_explosifs.png`, `bitcoin_bsadf_sequence.png`.

- **Sorties console** : tableaux récapitulatifs (stationnarité, Johansen, SADF/GSADF, épisodes datés) en français.

## Compilation du rapport

Le rapport `rapport_btc_bulle.tex` requiert un fichier `template.tex` dans le même dossier ainsi que les 8 PNG listés ci-dessus. Compilation standard avec `pdflatex` :

```
pdflatex rapport_btc_bulle.tex
```

## Notes méthodologiques

- **Lag du VAR** : sélectionné par AIC (\(p = 8\)). Le critère SC suggère \(p = 7\), conservé comme variante parcimonieuse possible.
- **Cointégration** : les tests de Phillips-Ouliaris et de Johansen divergent (PO rejette H0, Johansen ne la rejette pas au seuil 5 %). Le rapport retient le résultat de Johansen, standard en cadre multivarié.
- **SADF/GSADF** : implémentation par fallback `urca::ur.df` en boucle récursive, le package `MultipleBubbles` étant incompatible avec la version R utilisée. Les valeurs critiques utilisées (1.40 pour SADF, 1.87 pour GSADF) sont celles de Phillips, Shi & Yu (2015, Tables 1 et 2) pour \(T\) grand.

## Références principales

- Phillips, P. C. B., Shi, S., & Yu, J. (2015). "Testing for multiple bubbles: Historical episodes of exuberance and collapse in the S&P 500." *International Economic Review*, 56(4), 1043–1078.
- Bouoiyour, J., & Selmi, R. (2015). "What does Bitcoin look like?" *Annals of Economics and Finance*, 16(2), 449–492.
- Corbet, S., Lucey, B., & Yarovaya, L. (2018). "Datestamping the Bitcoin and Ethereum bubbles." *Finance Research Letters*, 26, 81–88.

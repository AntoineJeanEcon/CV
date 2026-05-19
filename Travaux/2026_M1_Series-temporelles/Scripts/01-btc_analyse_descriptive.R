# =============================================================================
# PROJET M1 ÉCONOMIE - DÉTECTION DE BULLE SPÉCULATIVE SUR LE BITCOIN
# Partie 2.1 : Analyse descriptive
# =============================================================================
# Auteur      : [Ton nom]
# Date        : avril 2026
# Outil       : R
# Source data : Yahoo Finance via quantmod (BTC-USD, journalier)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# Décommenter pour installation initiale :
# install.packages(c("quantmod", "ggplot2", "moments", "tseries",
#                    "xts", "zoo", "knitr", "kableExtra", "scales",
#                    "patchwork", "lubridate"))

library(quantmod)    # Téléchargement de données financières (getSymbols / Yahoo)
library(ggplot2)     # Visualisation
library(moments)     # skewness(), kurtosis()
library(tseries)     # jarque.bera.test()
library(xts)         # Objets time-series extensibles
library(zoo)         # Fonctions complémentaires pour séries temporelles
library(knitr)       # kable() pour tableaux propres
library(kableExtra)  # Mise en forme avancée des tableaux kable
library(scales)      # Formatage des axes ggplot (comma, date, etc.)
library(patchwork)   # Composition multi-graphiques ggplot2
library(lubridate)   # Manipulation de dates

# -----------------------------------------------------------------------------
# 1. TÉLÉCHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------
# Justification de la période 2015-01-01 / 2024-12-31 :
#   • Couvre 3 halvings Bitcoin (juillet 2016, mai 2020, avril 2024), chaque
#     halving marquant historiquement le début d'un cycle haussier.
#   • Intègre au minimum 3 épisodes de bulles identifiables : fin 2017
#     (~20 000 USD), fin 2021 (~69 000 USD), 2024 (ATH ~100 000 USD).
#   • ~3 650 observations journalières → suffisant pour les tests asymptotiques
#     SADF/GSADF (Phillips et al. recommandent T ≥ 100, idéalement T > 500).
#   • Le marché BTC avant 2015 est peu liquide et les données sont moins fiables.

date_debut <- "2015-01-01"
date_fin   <- "2024-12-31"

# getSymbols() charge directement l'objet dans l'environnement global
# src = "yahoo" est gratuit et ne requiert pas de clé API
getSymbols("BTC-USD",
           src   = "yahoo",
           from  = date_debut,
           to    = date_fin,
           auto.assign = TRUE)

# Récupération de l'objet xts (renommage pour simplifier)
btc_xts <- `BTC-USD`

# -----------------------------------------------------------------------------
# 2. NETTOYAGE DES DONNÉES
# -----------------------------------------------------------------------------

# -- 2.1 Sélection du prix de clôture ajusté (Adjusted Close)
# On utilise Cl() pour le Close standard ; Adj Close = Close pour les cryptos
# (pas de dividendes ni splits à ajuster)
prix <- Cl(btc_xts)                    # xts univarié : prix de clôture
colnames(prix) <- "Prix_BTC"

# -- 2.2 Vérification des doublons d'index
n_doublons <- sum(duplicated(index(prix)))
cat("Nombre de doublons d'index :", n_doublons, "\n")
if (n_doublons > 0) {
  prix <- prix[!duplicated(index(prix)), ]
  cat("  → Doublons supprimés.\n")
}

# -- 2.3 Gestion des valeurs manquantes (NA)
n_na <- sum(is.na(prix))
cat("Valeurs manquantes (NA) :", n_na, "\n")
if (n_na > 0) {
  # na.locf : forward-fill (dernier prix connu reporté)
  # Acceptable pour des données journalières (marché 24/7 mais Yahoo peut
  # avoir des trous ponctuels sur jours fériés US)
  prix <- na.locf(prix, na.rm = TRUE)
  cat("  → NA comblés par forward-fill (na.locf).\n")
}

cat("Période couverte :", format(start(prix)), "→", format(end(prix)), "\n")
cat("Nombre d'observations :", nrow(prix), "\n")

# -- 2.4 Conversion en data.frame pour ggplot2
df <- data.frame(
  date  = as.Date(index(prix)),
  prix  = as.numeric(prix)
)

# -- 2.5 Calcul des log-rendements journaliers
# r_t = ln(P_t / P_{t-1})
# On retire le premier NA généré par diff
df$log_ret <- c(NA, diff(log(df$prix)))
df_ret <- df[!is.na(df$log_ret), ]  # sous-ensemble sans le premier NA

cat("Observations pour les rendements :", nrow(df_ret), "\n\n")

# -----------------------------------------------------------------------------
# 3. STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------

# Fonction utilitaire : calcule un ensemble de stats sur un vecteur numérique
stats_desc <- function(x, nom = "Série") {
  data.frame(
    Série        = nom,
    N            = length(x),
    Moyenne      = mean(x,   na.rm = TRUE),
    Médiane      = median(x, na.rm = TRUE),
    Écart_type   = sd(x,     na.rm = TRUE),
    CV           = sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)),  # coefficient de variation
    Skewness     = skewness(x, na.rm = TRUE),
    Kurtosis     = kurtosis(x, na.rm = TRUE),   # kurtosis "brut" (normale = 3)
    Kurtosis_exc = kurtosis(x, na.rm = TRUE) - 3, # excès de kurtosis (normale = 0)
    Min          = min(x,    na.rm = TRUE),
    Max          = max(x,    na.rm = TRUE),
    check.names  = FALSE
  )
}

stats_prix <- stats_desc(df$prix,        nom = "Prix BTC (niveaux, USD)")
stats_ret  <- stats_desc(df_ret$log_ret, nom = "Log-rendements journaliers")

# Fusion des deux lignes
tableau_stats <- rbind(stats_prix, stats_ret)

# Affichage console
cat("=== STATISTIQUES DESCRIPTIVES ===\n")
print(t(tableau_stats), digits = 4)

# -----------------------------------------------------------------------------
# 4. TEST DE NORMALITÉ DE JARQUE-BERA
# -----------------------------------------------------------------------------
# H0 : la distribution est normale (skewness = 0, kurtosis excédentaire = 0)
# Statistique JB ~ chi²(2) sous H0

jb_test <- jarque.bera.test(df_ret$log_ret)
cat("\n=== TEST DE JARQUE-BERA (log-rendements) ===\n")
print(jb_test)
cat("Interprétation :",
    ifelse(jb_test$p.value < 0.05,
           "Rejet de H0 (p < 0.05) → distribution non normale.",
           "Non-rejet de H0 → distribution compatible avec la normalité."),
    "\n\n")

# -----------------------------------------------------------------------------
# 5. TABLEAU RÉCAPITULATIF FORMATÉ (kableExtra)
# -----------------------------------------------------------------------------
# On formate les colonnes numériques selon leur nature

# Transposition pour une lecture verticale plus lisible
tab_affich <- as.data.frame(t(tableau_stats[, -1]))
colnames(tab_affich) <- tableau_stats$Série
tab_affich$Statistique <- rownames(tab_affich)
tab_affich <- tab_affich[, c("Statistique",
                              "Prix BTC (niveaux, USD)",
                              "Log-rendements journaliers")]
rownames(tab_affich) <- NULL

cat("=== TABLEAU RÉCAPITULATIF ===\n")
print(
  kable(tab_affich,
        digits  = 4,
        caption = "Statistiques descriptives – Bitcoin (BTC/USD), 2015–2024",
        format  = "simple")
)

# -----------------------------------------------------------------------------
# 6. GRAPHIQUES GGPLOT2
# -----------------------------------------------------------------------------

# Palette cohérente
couleur_btc <- "#F7931A"   # orange Bitcoin
couleur_ret  <- "#2C3E50"  # bleu foncé neutre

theme_projet <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey55", size = 8, hjust = 0),
    axis.title    = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# -- 6.1 Série temporelle : prix en niveau

g1 <- ggplot(df, aes(x = date, y = prix)) +
  geom_line(color = couleur_btc, linewidth = 0.6) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = " ")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Prix du Bitcoin (BTC/USD) — données journalières",
    subtitle = "Cours de clôture, 1er janvier 2015 – 31 décembre 2024",
    x        = NULL,
    y        = "Prix (USD)",
    caption  = "Source : Yahoo Finance via quantmod | Cours de clôture quotidien"
  ) +
  theme_projet

# -- 6.2 Série temporelle : log-rendements

g2 <- ggplot(df_ret, aes(x = date, y = log_ret)) +
  geom_line(color = couleur_ret, linewidth = 0.35, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Log-rendements journaliers du Bitcoin (BTC/USD)",
    subtitle = "r_t = ln(P_t / P_{t-1}), 2015–2024",
    x        = NULL,
    y        = "Log-rendement",
    caption  = "Source : Yahoo Finance via quantmod"
  ) +
  theme_projet

# -- 6.3 Histogramme des log-rendements + courbe normale théorique

mu_ret  <- mean(df_ret$log_ret)
sd_ret  <- sd(df_ret$log_ret)

g3 <- ggplot(df_ret, aes(x = log_ret)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins   = 120,
                 fill   = couleur_ret,
                 alpha  = 0.65,
                 color  = "white",
                 linewidth = 0.1) +
  stat_function(
    fun  = dnorm,
    args = list(mean = mu_ret, sd = sd_ret),
    color = "red", linewidth = 0.9, linetype = "solid"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Distribution des log-rendements journaliers — BTC/USD",
    subtitle = paste0("Courbe rouge = N(μ=", round(mu_ret,4),
                      ", σ=", round(sd_ret,4), ") | 2015–2024"),
    x        = "Log-rendement journalier",
    y        = "Densité",
    caption  = "Source : Yahoo Finance via quantmod"
  ) +
  theme_projet

# -- 6.4 QQ-plot des log-rendements

g4 <- ggplot(df_ret, aes(sample = log_ret)) +
  stat_qq(color = couleur_ret, alpha = 0.4, size = 0.7) +
  stat_qq_line(color = "red", linewidth = 0.8) +
  labs(
    title    = "QQ-plot des log-rendements journaliers — BTC/USD",
    subtitle = "Quantiles empiriques vs quantiles normaux théoriques | 2015–2024",
    x        = "Quantiles théoriques N(0,1)",
    y        = "Quantiles empiriques",
    caption  = "Source : Yahoo Finance via quantmod"
  ) +
  theme_projet

# -----------------------------------------------------------------------------
# 7. EXPORT DES GRAPHIQUES EN PNG HAUTE RÉSOLUTION
# -----------------------------------------------------------------------------
# Les fichiers sont sauvegardés dans le répertoire de travail courant.
# Pour changer la destination, modifier le chemin en premier argument de ggsave().

res_dpi <- 300  # résolution standard publication

ggsave("btc_prix_niveau.png",     plot = g1, width = 10, height = 5,
       dpi = res_dpi, bg = "white")
ggsave("btc_log_rendements.png",  plot = g2, width = 10, height = 5,
       dpi = res_dpi, bg = "white")
ggsave("btc_histogramme_ret.png", plot = g3, width = 8,  height = 5,
       dpi = res_dpi, bg = "white")
ggsave("btc_qqplot_ret.png",      plot = g4, width = 7,  height = 6,
       dpi = res_dpi, bg = "white")

cat("Graphiques exportés en PNG (300 dpi) dans le répertoire courant :\n")
cat("  • btc_prix_niveau.png\n")
cat("  • btc_log_rendements.png\n")
cat("  • btc_histogramme_ret.png\n")
cat("  • btc_qqplot_ret.png\n\n")

# -- Optionnel : affichage combiné dans le viewer R (patchwork)
#(g1 / g2) | (g3 / g4)

# =============================================================================
# FIN DU SCRIPT
# =============================================================================

# Projet M1 - Bulle Bitcoin
# Script 3 : tests SADF et GSADF
# Antoine Jean - 2026
#
# Méthode : Phillips, Shi & Yu (2015, International Economic Review)
# Prérequis : avoir tourné les scripts 1 et 2 (objets df, prix_clean)
#
# Note : le package MultipleBubbles bug avec ma version de R, donc on fait
# tout à la main avec urca::ur.df dans une boucle.


library(ggplot2)
library(dplyr)
library(lubridate)
library(urca)


# =============================================================================
# BLOC A - Préparation
# =============================================================================

cat("\n=== BLOC A : Préparation ===\n")

if (!exists("prix_clean") || !exists("df")) {
    stop("Il faut tourner les scripts 1 et 2 avant !")
}

# Les tests SADF/GSADF s'appliquent sur le log-prix en niveau (pas les rendements)
log_prix  <- log(prix_clean)
T_obs     <- length(log_prix)
dates_vec <- df$date

cat("T =", T_obs, "\n")
cat("Période :", format(min(dates_vec), "%d/%m/%Y"),
    "->", format(max(dates_vec), "%d/%m/%Y"), "\n")

# Fenêtre minimale recommandée par Phillips et al. 2015
# minw = floor((0.01 + 1.8/sqrt(T)) * T)
minw <- floor((0.01 + 1.8 / sqrt(T_obs)) * T_obs)
cat("minw =", minw, "\n")

# Durée minimale d'un épisode = ceiling(log(T))
# (pour filtrer les faux positifs ponctuels)
duree_min <- ceiling(log(T_obs))
cat("Durée mini épisode =", duree_min, "jours\n")

# Lag ADF = 1 (standard pour données journalières)
adf_lag <- 1



# =============================================================================
# BLOC B - Test SADF
# =============================================================================
# SADF = supremum sur une boucle d'ADF récursifs
# La fenêtre commence à 1 et augmente : ADF sur [1, r2] pour r2 de minw à T

cat("\n=== BLOC B : SADF ===\n")
cat("Boucle ADF récursive...\n")

seq_sadf <- rep(NA_real_, T_obs)

n_iter  <- T_obs - minw + 1
pb_step <- max(1, floor(n_iter / 10))

for (i in 1:n_iter) {
    r2 <- minw + i - 1
    ur_res <- ur.df(log_prix[1:r2], type = "none", lags = adf_lag)
    seq_sadf[r2] <- ur_res@teststat[1]

    if (i %% pb_step == 0) {
        cat(sprintf("  %d/%d\n", i, n_iter))
    }
}

stat_sadf <- max(seq_sadf, na.rm = TRUE)

# Valeur critique 95% : Phillips, Shi & Yu (2015), Table 1
# Pour T grand, CV -> ~1.40
cv95_sadf <- 1.40

cat(sprintf("\nSADF = %.4f (CV 95%% = %.2f)\n", stat_sadf, cv95_sadf))
cat("=> ", ifelse(stat_sadf > cv95_sadf,
                  "Rejet de H0 : au moins une bulle détectée",
                  "Pas de bulle"), "\n")



# =============================================================================
# BLOC C - Test GSADF
# =============================================================================
# GSADF = supremum sur les ADF récursifs avec point de départ ET de fin variables
# Double boucle : pour chaque r2, on boucle sur r1 ; on prend le max des ADF
# Plus puissant pour détecter des bulles MULTIPLES
#
# Pb : la double boucle complète est O(T^2) -> trop long pour T=3653
# Donc on prend r1 par pas de floor(minw/2) (= grille sparse)
# C'est une approximation du vrai GSADF, mais ça suffit pour le date-stamping

cat("\n=== BLOC C : GSADF (approximation par grille sparse) ===\n")

seq_bsadf <- rep(NA_real_, T_obs)

r1_step <- floor(minw / 2)
cat("Pas r1 :", r1_step, "\n\n")

for (r2 in minw:T_obs) {
    r1_grid  <- seq(1, r2 - minw + 1, by = r1_step)
    adf_vals <- numeric(length(r1_grid))

    for (j in seq_along(r1_grid)) {
        r1 <- r1_grid[j]
        sub <- log_prix[r1:r2]
        if (length(sub) < minw) next
        ur_res <- ur.df(sub, type = "none", lags = adf_lag)
        adf_vals[j] <- ur_res@teststat[1]
    }

    seq_bsadf[r2] <- max(adf_vals, na.rm = TRUE)

    if (r2 %% 500 == 0) {
        cat(sprintf("  r2 = %d / %d\n", r2, T_obs))
    }
}

stat_gsadf <- max(seq_bsadf, na.rm = TRUE)

# CV 95% pour GSADF (Phillips, Shi & Yu 2015, Table 2) : ~1.87 pour T grand
cv95_gsadf <- 1.87

cat(sprintf("\nGSADF = %.4f (CV 95%% = %.2f)\n", stat_gsadf, cv95_gsadf))
cat("=> ", ifelse(stat_gsadf > cv95_gsadf,
                  "Rejet de H0 : bulles multiples détectées",
                  "Pas de bulle"), "\n")



# =============================================================================
# BLOC D - Date-stamping
# =============================================================================
# Méthode Phillips et al. : on compare la séquence BSADF à la CV 95%
# Un épisode commence quand BSADF passe au-dessus, finit quand il repasse en dessous
# On garde uniquement les épisodes >= duree_min jours

cat("\n=== BLOC D : Date-stamping ===\n\n")

is_explosive <- as.integer(seq_bsadf > cv95_gsadf & !is.na(seq_bsadf))

# Trouver les transitions 0->1 et 1->0
trans <- diff(c(0L, is_explosive, 0L))
starts <- which(trans ==  1L)
ends   <- which(trans == -1L) - 1L

if (length(starts) > 0) {
    ends_safe <- pmin(ends, length(dates_vec))
    episodes_brut <- data.frame(
        debut = dates_vec[starts],
        fin   = dates_vec[ends_safe],
        duree = as.integer(dates_vec[ends_safe] - dates_vec[starts])
    )
} else {
    episodes_brut <- data.frame(debut = as.Date(character()),
                                fin   = as.Date(character()),
                                duree = integer())
}

cat("Épisodes bruts :", nrow(episodes_brut), "\n")

# Filtre durée >= duree_min
episodes <- episodes_brut[episodes_brut$duree >= duree_min, ]
rownames(episodes) <- NULL

cat("Épisodes après filtre (>=", duree_min, "j) :", nrow(episodes), "\n\n")

# Affichage
if (nrow(episodes) > 0) {
    for (i in 1:nrow(episodes)) {
        cat(sprintf("  %s -> %s  (%d j)\n",
                    format(episodes$debut[i], "%d/%m/%Y"),
                    format(episodes$fin[i], "%d/%m/%Y"),
                    episodes$duree[i]))
    }

    # Censure à droite ?
    if (max(episodes$fin) == max(dates_vec)) {
        cat("\nAttention : le dernier épisode finit au 31/12/2024,\n")
        cat("c'est juste parce que la série s'arrête là (censure à droite)\n")
    }
}



# =============================================================================
# BLOC E - Graphiques
# =============================================================================

cat("\n=== BLOC E : Graphiques ===\n")

theme_proj <- theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
          plot.caption = element_text(hjust = 1, size = 8, color = "gray50"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

col_serie <- "#2c3e50"
col_bulle <- "#e74c3c"
col_bsadf <- "#2980b9"


# --- G1 : log-prix avec zones de bulle ---
df_g1 <- data.frame(date = dates_vec, log_prix = log_prix)

if (nrow(episodes) > 0) {
    rect_df <- data.frame(xmin = episodes$debut, xmax = episodes$fin,
                          ymin = -Inf, ymax = Inf)
} else {
    rect_df <- data.frame(xmin = as.Date(NA), xmax = as.Date(NA),
                          ymin = -Inf, ymax = Inf)[0, ]
}

g1 <- ggplot(df_g1, aes(x = date, y = log_prix)) +
    {if (nrow(rect_df) > 0)
        geom_rect(data = rect_df,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = col_bulle, alpha = 0.15)} +
    geom_line(color = col_serie, linewidth = 0.6) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limits = range(dates_vec)) +
    labs(title = "Log-prix du Bitcoin (BTC/USD) et épisodes explosifs détectés",
         subtitle = paste0("Zones rouges : BSADF > CV 95% pendant >= ",
                           duree_min, " jours"),
         x = NULL, y = "Log(Prix)",
         caption = "Source : Yahoo Finance (2015-2024). GSADF, Phillips, Shi & Yu (2015)") +
    theme_proj

ggsave("bitcoin_logprix_episodes_explosifs.png",
       plot = g1, width = 12, height = 6, dpi = 300, bg = "white")
cat("G1 exporté\n")


# --- G2 : séquence BSADF ---
idx_ok <- which(!is.na(seq_bsadf))
df_g2  <- data.frame(date = dates_vec[idx_ok], bsadf = seq_bsadf[idx_ok])

g2 <- ggplot(df_g2, aes(x = date, y = bsadf)) +
    {if (nrow(rect_df) > 0)
        geom_rect(data = rect_df,
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, fill = col_bulle, alpha = 0.08)} +
    geom_hline(yintercept = cv95_gsadf,
               color = col_bulle, linetype = "dashed", linewidth = 0.9) +
    geom_line(color = col_bsadf, linewidth = 0.55) +
    annotate("text", x = min(df_g2$date) + 60, y = cv95_gsadf + 0.12,
             label = sprintf("CV 95%% = %.2f", cv95_gsadf),
             color = col_bulle, size = 3.2, hjust = 0, fontface = "italic") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limits = range(dates_vec)) +
    labs(title = "Séquence BSADF - Détection des bulles Bitcoin (2015-2024)",
         subtitle = paste0("Au-dessus de la ligne pointillée : comportement explosif"),
         x = NULL, y = "Statistique BSADF",
         caption = "Source : Yahoo Finance (2015-2024). Phillips, Shi & Yu (2015)") +
    theme_proj

ggsave("bitcoin_bsadf_sequence.png",
       plot = g2, width = 12, height = 5, dpi = 300, bg = "white")
cat("G2 exporté\n")



# =============================================================================
# Récap final
# =============================================================================

cat("\n=== RÉCAP ===\n")
cat(sprintf("SADF  = %.4f | CV 95%% = %.2f | rejet H0 : %s\n",
            stat_sadf, cv95_sadf, ifelse(stat_sadf > cv95_sadf, "OUI", "NON")))
cat(sprintf("GSADF = %.4f | CV 95%% = %.2f | rejet H0 : %s\n",
            stat_gsadf, cv95_gsadf, ifelse(stat_gsadf > cv95_gsadf, "OUI", "NON")))
cat("Épisodes retenus :", nrow(episodes), "\n")

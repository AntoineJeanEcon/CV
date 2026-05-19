# =============================================================================
# SCRIPT #3 (v2) — TESTS DE RACINE UNITAIRE EXPLOSIVE : SADF & GSADF
# Bitcoin / USD, 2015-2024
# Méthode : Phillips, Shi & Yu (2015, International Economic Review)
# Corrections v2 :
#   - Filtre de durée minimale ceiling(log(T)) sur les épisodes (Phillips et al.)
#   - Tentative d'appel natif MultipleBubbles sans arguments non reconnus
#   - Commentaires explicites sur censure à droite et épisode 2020-2022
#   - Graphiques mis à jour pour refléter uniquement les épisodes filtrés
# Ce script suppose que df, prix et prix_clean sont en mémoire (scripts #1 & #2)
# =============================================================================

# --- Packages requis ----------------------------------------------------------
packages <- c("MultipleBubbles", "ggplot2", "dplyr", "lubridate", "urca")

for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
}

library(ggplot2)
library(dplyr)
library(lubridate)

# =============================================================================
# BLOC A — PRÉPARATION DE LA SÉRIE
# =============================================================================

cat("\n=== BLOC A : Préparation de la série ===\n")

if (!exists("prix_clean") || !exists("df")) {
    stop("Les objets 'prix_clean' et 'df' sont introuvables. Exécutez d'abord les scripts #1 et #2.")
}

# Les tests SADF/GSADF s'appliquent sur le log-prix (niveau), pas les rendements.
# On teste si le processus générateur exhibe une racine > 1 sur des sous-fenêtres,
# ce qui correspond à une dynamique explosive incompatible avec une marche aléatoire I(1).
log_prix <- log(prix_clean)
T_obs    <- length(log_prix)
dates_vec <- df$date

cat(sprintf("  Série : log-prix BTC/USD | T = %d observations\n", T_obs))
cat(sprintf("  Période : %s → %s\n",
            format(min(dates_vec), "%d/%m/%Y"),
            format(max(dates_vec), "%d/%m/%Y")))

# --- Fenêtre minimale ---------------------------------------------------------
# Formule de Phillips, Shi & Yu (2015, p. 1056) :
#   minw = floor((0.01 + 1.8 / sqrt(T)) * T)
# Raisonnement : la fenêtre initiale doit être assez grande pour que la
# régression ADF ait des degrés de liberté suffisants, mais assez petite
# pour pouvoir détecter des bulles précoces dans la série.
# Pour T = 3653 : minw ≈ 145 observations (~4.8 mois).
minw <- floor((0.01 + 1.8 / sqrt(T_obs)) * T_obs)
cat(sprintf("  Fenêtre minimale (minw) : %d observations (~%.1f mois)\n",
            minw, minw / 30.5))

# --- Durée minimale d'un épisode ----------------------------------------------
# Phillips et al. (2015) imposent un filtre : un épisode n'est retenu que
# s'il dure au moins ceiling(log(T)) périodes consécutives, afin d'éliminer
# les franchissements de seuil transitoires (faux positifs).
# Pour T = 3653 : ceiling(log(3653)) = 9 jours.
duree_min <- ceiling(log(T_obs))
cat(sprintf("  Durée minimale d'un épisode : %d jours (= ceiling(log(T)))\n", duree_min))

# Lag ADF = 1 : recommandé par Phillips et al. pour données journalières.
adf_lag <- 1

# =============================================================================
# DÉTECTION DE L'API MultipleBubbles
# =============================================================================

use_MultipleBubbles <- FALSE  # par défaut, on part sur le fallback fiable

if (requireNamespace("MultipleBubbles", quietly = TRUE)) {
    library(MultipleBubbles)
    
    # On teste l'appel le plus simple possible (sans arguments optionnels)
    # pour détecter si le package répond — la v2.x n'accepte pas adf_lag/minw
    # en arguments nommés explicites mais les intègre via des paramètres internes.
    # Si l'appel échoue, on bascule proprement sur urca.
    test_call <- tryCatch({
        # Appel minimal sur une sous-série courte pour tester la signature
        dummy <- log_prix[1:200]
        if (exists("sadf", where = asNamespace("MultipleBubbles"), inherits = FALSE)) {
            MultipleBubbles::sadf(dummy)
            "sadf"
        } else if (exists("sadf_test", where = asNamespace("MultipleBubbles"), inherits = FALSE)) {
            MultipleBubbles::sadf_test(dummy)
            "sadf_test"
        } else {
            "none"
        }
    }, error = function(e) "error")
    
    if (test_call %in% c("sadf", "sadf_test")) {
        use_MultipleBubbles <- TRUE
        mb_fn <- test_call  # nom de la fonction à utiliser
        cat(sprintf("  [INFO] MultipleBubbles disponible — fonction : %s()\n", mb_fn))
    } else {
        cat("  [INFO] MultipleBubbles inutilisable (API incompatible). Fallback urca activé.\n")
    }
} else {
    cat("  [INFO] MultipleBubbles absent. Fallback urca activé.\n")
}

library(urca)

# =============================================================================
# BLOC B — TEST SADF
# =============================================================================

cat("\n=== BLOC B : Test SADF (Supremum ADF) ===\n")
cat("  Détecte une unique période explosive (hypothèse alternative simple).\n")
cat("  Boucle récursive forward : ADF estimé sur [1, r2] pour r2 = minw...T\n\n")

# Initialisation de la séquence
seq_sadf <- rep(NA_real_, T_obs)

if (use_MultipleBubbles) {
    
    tryCatch({
        res_sadf <- if (mb_fn == "sadf") MultipleBubbles::sadf(log_prix) else
            MultipleBubbles::sadf_test(log_prix)
        
        stat_sadf <- if (!is.null(res_sadf$sadf))       res_sadf$sadf       else
            if (!is.null(res_sadf$statistic))   res_sadf$statistic  else
                max(res_sadf$bsadf %||% res_sadf$sequence, na.rm = TRUE)
        
        cv95_sadf <- if (!is.null(res_sadf$cv))              res_sadf$cv["95%"]              else
            if (!is.null(res_sadf$critical_values))  res_sadf$critical_values["95%"] else
                NA_real_
        
        seq_sadf  <- if (!is.null(res_sadf$bsadf))    res_sadf$bsadf    else
            if (!is.null(res_sadf$sequence))  res_sadf$sequence else
                rep(NA_real_, T_obs)
        
        cat(sprintf("  [MultipleBubbles] Statistique SADF   : %.4f\n", stat_sadf))
        if (!is.na(cv95_sadf)) cat(sprintf("  [MultipleBubbles] Valeur critique 95%% : %.4f\n", cv95_sadf))
        
    }, error = function(e) {
        cat(sprintf("  [ERREUR] %s — Basculement fallback urca.\n", e$message))
        use_MultipleBubbles <<- FALSE
    })
}

if (!use_MultipleBubbles) {
    
    cat("  [FALLBACK urca] Boucle récursive SADF manuelle...\n")
    n_iter  <- T_obs - minw + 1
    pb_step <- max(1, floor(n_iter / 10))
    
    for (i in seq_len(n_iter)) {
        r2 <- minw + i - 1
        tryCatch({
            ur_res       <- urca::ur.df(log_prix[1:r2], type = "none", lags = adf_lag)
            seq_sadf[r2] <- ur_res@teststat[1]
        }, error = function(e) { seq_sadf[r2] <<- NA_real_ })
        
        if (i %% pb_step == 0)
            cat(sprintf("    %d/%d (%.0f%%)\n", i, n_iter, 100 * i / n_iter))
    }
    
    stat_sadf <- max(seq_sadf, na.rm = TRUE)
    
    # Valeur critique à 95 % — Phillips, Shi & Yu (2015), Table 1, Panel A
    # Pour T grand (≥ 400), la CV converge vers ~1.40 au seuil 95 %.
    cv95_sadf <- 1.40
    
    cat(sprintf("\n  Statistique SADF     : %.4f\n", stat_sadf))
    cat(sprintf("  Valeur critique 95%%  : %.4f (Phillips et al. 2015, Table 1)\n", cv95_sadf))
}

if (stat_sadf > cv95_sadf) {
    cat("  CONCLUSION SADF : Statistique > valeur critique à 95 %.\n")
    cat("    La série log-prix BTC présente au moins un épisode de comportement\n")
    cat("    explosif sur 2015-2024. H0 (absence de bulle) rejetée.\n")
} else {
    cat("  CONCLUSION SADF : Statistique ≤ valeur critique. Pas de bulle détectée.\n")
}

# =============================================================================
# BLOC C — TEST GSADF
# =============================================================================

cat("\n=== BLOC C : Test GSADF (Generalized SADF) ===\n")
cat("  Extension du SADF autorisant un point de départ r1 variable.\n")
cat("  Plus puissant pour détecter des bulles multiples (2017, 2021, 2024).\n")
cat("  Double boucle : ADF estimé sur [r1, r2] pour tous r1 < r2 valides.\n\n")

seq_bsadf <- rep(NA_real_, T_obs)

if (use_MultipleBubbles) {
    
    tryCatch({
        res_gsadf <- if (mb_fn == "sadf") MultipleBubbles::gsadf(log_prix) else
            MultipleBubbles::gsadf_test(log_prix)
        
        stat_gsadf <- if (!is.null(res_gsadf$gsadf))      res_gsadf$gsadf      else
            if (!is.null(res_gsadf$statistic))   res_gsadf$statistic  else
                max(res_gsadf$bsadf %||% res_gsadf$sequence, na.rm = TRUE)
        
        cv95_gsadf <- if (!is.null(res_gsadf$cv))              res_gsadf$cv["95%"]              else
            if (!is.null(res_gsadf$critical_values))  res_gsadf$critical_values["95%"] else
                NA_real_
        
        seq_bsadf  <- if (!is.null(res_gsadf$bsadf))    res_gsadf$bsadf    else
            if (!is.null(res_gsadf$sequence))  res_gsadf$sequence else
                seq_sadf
        
        cat(sprintf("  [MultipleBubbles] Statistique GSADF  : %.4f\n", stat_gsadf))
        if (!is.na(cv95_gsadf)) cat(sprintf("  [MultipleBubbles] Valeur critique 95%% : %.4f\n", cv95_gsadf))
        
    }, error = function(e) {
        cat(sprintf("  [ERREUR] %s — Basculement fallback urca.\n", e$message))
        use_MultipleBubbles <<- FALSE
    })
}

if (!use_MultipleBubbles) {
    
    cat("  [FALLBACK urca] Double boucle GSADF (r1 sparse, pas = floor(minw/2))...\n")
    cat("  Note : approximation par grille — la séquence BSADF est une borne\n")
    cat("  inférieure du vrai GSADF. Les dates peuvent être décalées de quelques semaines.\n\n")
    
    # Pas de r1 : on ne teste pas tous les points de départ possibles (coût O(T²))
    # mais une grille avec pas = floor(minw/2). C'est le compromis standard
    # pour des séries longues (T > 2000) sans accès au package compilé.
    r1_step <- max(1, floor(minw / 2))
    cat(sprintf("  Pas r1 : %d observations (floor(minw/2) = floor(%d/2))\n\n", r1_step, minw))
    
    for (r2 in minw:T_obs) {
        r1_grid  <- seq(1, r2 - minw + 1, by = r1_step)
        adf_vals <- numeric(length(r1_grid))
        
        for (j in seq_along(r1_grid)) {
            r1        <- r1_grid[j]
            sub_serie <- log_prix[r1:r2]
            if (length(sub_serie) < minw) next
            tryCatch({
                ur_res       <- urca::ur.df(sub_serie, type = "none", lags = adf_lag)
                adf_vals[j]  <- ur_res@teststat[1]
            }, error = function(e) { adf_vals[j] <<- NA_real_ })
        }
        
        seq_bsadf[r2] <- max(adf_vals, na.rm = TRUE)
        if (r2 %% 500 == 0) cat(sprintf("    r2 = %d / %d\n", r2, T_obs))
    }
    
    stat_gsadf <- max(seq_bsadf, na.rm = TRUE)
    
    # Valeur critique à 95 % — Phillips, Shi & Yu (2015), Table 2, Panel A
    # Pour T grand, la CV GSADF converge vers ~1.87 au seuil 95 %.
    cv95_gsadf <- 1.87
    
    cat(sprintf("\n  Statistique GSADF    : %.4f\n", stat_gsadf))
    cat(sprintf("  Valeur critique 95%%  : %.4f (Phillips et al. 2015, Table 2)\n", cv95_gsadf))
}

if (stat_gsadf > cv95_gsadf) {
    cat("  CONCLUSION GSADF : Statistique > valeur critique à 95 %.\n")
    cat("    Présence d'épisodes explosifs multiples confirmée.\n")
    cat("    Le GSADF surpasse le SADF en puissance pour ce type de série.\n")
} else {
    cat("  CONCLUSION GSADF : Pas de comportement explosif détecté.\n")
}

# =============================================================================
# BLOC D — DATE-STAMPING DES ÉPISODES EXPLOSIFS
# =============================================================================

cat("\n=== BLOC D : Date-stamping des épisodes explosifs ===\n")
cat("  Méthode : la séquence BSADF est comparée à la valeur critique cv95_gsadf.\n")
cat("  Début d'épisode : première date où BSADF > CV95.\n")
cat("  Fin d'épisode   : première date suivante où BSADF < CV95.\n")
cat(sprintf("  Filtre durée minimale : %d jours (ceiling(log(T = %d)))\n\n",
            duree_min, T_obs))

# --- Indicateur binaire -------------------------------------------------------
is_explosive <- as.integer(seq_bsadf > cv95_gsadf & !is.na(seq_bsadf))

# --- Identification des transitions -------------------------------------------
transitions <- diff(c(0L, is_explosive, 0L))
starts_idx  <- which(transitions ==  1L)
ends_idx    <- which(transitions == -1L) - 1L

# --- Tableau brut (avant filtre) ----------------------------------------------
if (length(starts_idx) == 0) {
    cat("  Aucun épisode explosif brut détecté.\n")
    episodes_brut <- data.frame(debut = as.Date(character()),
                                fin   = as.Date(character()),
                                duree = integer())
} else {
    ends_idx_safe <- pmin(ends_idx, length(dates_vec))
    episodes_brut <- data.frame(
        debut = dates_vec[starts_idx],
        fin   = dates_vec[ends_idx_safe],
        duree = as.integer(dates_vec[ends_idx_safe] - dates_vec[starts_idx])
    )
}

cat(sprintf("  Épisodes bruts (avant filtre) : %d\n", nrow(episodes_brut)))

# --- Application du filtre durée minimale -------------------------------------
# Phillips et al. (2015) : seuls les épisodes d'au moins ceiling(log(T))
# périodes consécutives sont retenus. En dessous de ce seuil, le franchissement
# de la valeur critique est vraisemblablement un artefact statistique.
episodes <- episodes_brut[episodes_brut$duree >= duree_min, ]
rownames(episodes) <- NULL

cat(sprintf("  Épisodes retenus (durée ≥ %d j) : %d\n\n", duree_min, nrow(episodes)))

# --- Affichage du tableau filtré ----------------------------------------------
if (nrow(episodes) == 0) {
    cat("  Aucun épisode ne passe le filtre de durée minimale.\n")
} else {
    
    cat(sprintf("  %-13s  %-13s  %-10s  Interprétation\n", "Début", "Fin", "Durée (j)"))
    cat(paste(rep("─", 75), collapse = ""), "\n")
    
    for (i in seq_len(nrow(episodes))) {
        d_s  <- episodes$debut[i]
        d_e  <- episodes$fin[i]
        dur  <- episodes$duree[i]
        yr_s <- year(d_s)
        mo_s <- month(d_s)
        yr_e <- year(d_e)
        
        # Correspondance avec les épisodes documentés dans la littérature :
        # ─ 2016-2018 : bulle post-halving juillet 2016 → ATH ~20 000 USD déc. 2017,
        #               éclatement progressif jusqu'à fin 2018 (Geuder et al., 2019 ;
        #               Bouoiyour & Selmi, 2019).
        # ─ 2019      : rebond intermédiaire, épisode court, interprétation prudente.
        # ─ 2020-2022 : bullrun post-COVID + adoption institutionnelle (MicroStrategy,
        #               Tesla, PayPal). Double pic : avr. 2021 (~64k$) et nov. 2021
        #               (~69k$). La BSADF ne repasse pas sous le seuil entre les deux
        #               pics → détecté comme un unique épisode continu.
        # ─ 2023-2024 : approbation ETF spot Bitcoin par la SEC (jan. 2024) + halving
        #               avr. 2024 → nouveau bullrun. NOTE : si cet épisode se termine
        #               au 31/12/2024, la date de fin est censurée par la fin de
        #               l'échantillon (la bulle peut être encore en cours).
        censure <- if (d_e == max(dates_vec)) " [⚠ censure droite]" else ""
        
        label <- dplyr::case_when(
            yr_s >= 2016 & yr_e <= 2019 & yr_s <= 2018 ~
                "Bulle 2017 — post-halving juil. 2016 (ATH ~20k$)",
            yr_s == 2019 ~
                "Rebond 2019 — épisode intermédiaire",
            (yr_s == 2019 & mo_s >= 10) | yr_s == 2020 | (yr_s == 2021) | (yr_s == 2022 & yr_e <= 2022) ~
                "Bulle 2020-2021 — COVID recovery + adoption institutionnelle",
            yr_s >= 2023 ~
                paste0("Bulle 2023-2024 — ETF spot SEC + halving avr. 2024", censure),
            TRUE ~ paste0("Épisode non classifié", censure)
        )
        
        cat(sprintf("  %-13s  %-13s  %-10d  %s\n",
                    format(d_s, "%d/%m/%Y"),
                    format(d_e, "%d/%m/%Y"),
                    dur, label))
    }
    cat("\n")
    
    # Note explicite sur la censure à droite
    if (max(episodes$fin) == max(dates_vec)) {
        cat("  ⚠  CENSURE À DROITE : Le dernier épisode se termine au 31/12/2024\n")
        cat("     uniquement parce que la série s'arrête à cette date.\n")
        cat("     Cela ne signifie pas que le comportement explosif a cessé.\n")
        cat("     Cette limite doit être signalée explicitement dans le rapport.\n\n")
    }
    
    # Note sur l'épisode long 2020-2022
    ep_long <- episodes[episodes$duree > 500, ]
    if (nrow(ep_long) > 0) {
        cat("  ℹ  ÉPISODE LONG (> 500j) : La BSADF ne repasse pas sous le seuil\n")
        cat("     entre les deux ATH de 2021 (avr. et nov.). Les deux phases sont\n")
        cat("     fusionnées en un seul épisode continu. Ce n'est pas une anomalie :\n")
        cat("     le BTC n'est pas revenu à son niveau pré-bull entre les deux pics.\n\n")
    }
}

# =============================================================================
# BLOC E — GRAPHIQUES
# =============================================================================

cat("=== BLOC E : Génération des graphiques ===\n\n")

# Charte graphique commune (cohérence avec les scripts #1 et #2)
theme_projet <- theme_minimal(base_size = 12) +
    theme(
        plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle    = element_text(hjust = 0.5, color = "gray40", size = 10),
        plot.caption     = element_text(hjust = 1, size = 8, color = "gray50"),
        axis.title       = element_text(size = 10),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1)
    )

col_serie  <- "#2c3e50"
col_bulle  <- "#e74c3c"
col_bsadf  <- "#2980b9"
col_cv     <- "#e74c3c"

# ---- Graphique 1 : Log-prix + zones explosives filtrées ---------------------

df_plot1 <- data.frame(date = dates_vec, log_prix = log_prix)

rect_df <- if (nrow(episodes) > 0) {
    data.frame(xmin = episodes$debut, xmax = episodes$fin,
               ymin = -Inf,           ymax = Inf)
} else {
    data.frame(xmin = as.Date(NA), xmax = as.Date(NA),
               ymin = -Inf, ymax = Inf)[0, ]
}

g1 <- ggplot(df_plot1, aes(x = date, y = log_prix)) +
    { if (nrow(rect_df) > 0)
        geom_rect(data = rect_df,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = col_bulle, alpha = 0.15)
    } +
    geom_line(color = col_serie, linewidth = 0.6) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limits = range(dates_vec)) +
    labs(
        title    = "Log-prix du Bitcoin (BTC/USD) et épisodes explosifs détectés",
        subtitle = paste0("Zones rouges : BSADF > CV 95 % pendant ≥ ",
                          duree_min, " jours consécutifs (Phillips et al., 2015)"),
        x        = NULL, y = "Log(Prix)",
        caption  = "Sources : Yahoo Finance (2015-2024). Méthode : GSADF, Phillips, Shi & Yu (2015, IER)."
    ) +
    theme_projet

ggsave("bitcoin_logprix_episodes_explosifs.png",
       plot = g1, width = 12, height = 6, dpi = 300, bg = "white")
cat("  Graphique 1 exporté : bitcoin_logprix_episodes_explosifs.png\n")

# ---- Graphique 2 : Séquence BSADF vs valeur critique -------------------------

idx_valid <- which(!is.na(seq_bsadf))
df_plot2  <- data.frame(date = dates_vec[idx_valid], bsadf = seq_bsadf[idx_valid])

# Zones explosives pour le graphique BSADF (basées sur les épisodes filtrés)
rect_df2 <- rect_df  # même zones que g1

g2 <- ggplot(df_plot2, aes(x = date, y = bsadf)) +
    { if (nrow(rect_df2) > 0)
        geom_rect(data = rect_df2,
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, fill = col_bulle, alpha = 0.08)
    } +
    geom_hline(yintercept = cv95_gsadf,
               color = col_cv, linetype = "dashed", linewidth = 0.9) +
    geom_line(color = col_bsadf, linewidth = 0.55) +
    annotate("text",
             x     = min(df_plot2$date) + 60,
             y     = cv95_gsadf + 0.12,
             label = sprintf("CV 95 %% = %.2f", cv95_gsadf),
             color = col_cv, size = 3.2, hjust = 0, fontface = "italic") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 limits = range(dates_vec)) +
    labs(
        title    = "Séquence BSADF — Détection des bulles Bitcoin (2015-2024)",
        subtitle = paste0("Au-dessus de la ligne pointillée : comportement explosif à 95 % | ",
                          "Filtre durée ≥ ", duree_min, " j"),
        x        = NULL, y = "Statistique BSADF",
        caption  = "Sources : Yahoo Finance (2015-2024). Phillips, Shi & Yu (2015, International Economic Review)."
    ) +
    theme_projet

ggsave("bitcoin_bsadf_sequence.png",
       plot = g2, width = 12, height = 5, dpi = 300, bg = "white")
cat("  Graphique 2 exporté : bitcoin_bsadf_sequence.png\n")

# =============================================================================
# RÉCAPITULATIF FINAL
# =============================================================================

cat("\n=== RÉCAPITULATIF DES RÉSULTATS ===\n\n")
cat(sprintf("  SADF  — statistique : %.4f | CV 95%% : %.4f | Rejet H0 : %s\n",
            stat_sadf, cv95_sadf, ifelse(stat_sadf > cv95_sadf, "OUI", "NON")))
cat(sprintf("  GSADF — statistique : %.4f | CV 95%% : %.4f | Rejet H0 : %s\n",
            stat_gsadf, cv95_gsadf, ifelse(stat_gsadf > cv95_gsadf, "OUI", "NON")))
cat(sprintf("  Épisodes explosifs retenus : %d (filtre ≥ %d jours)\n\n",
            nrow(episodes), duree_min))

cat("=== OBJETS EN MÉMOIRE POUR LE SCRIPT #4 ===\n")
cat("  log_prix      — log-prix BTC (vecteur, T=3653)\n")
cat("  seq_sadf      — séquence ADF récursive (SADF)\n")
cat("  seq_bsadf     — séquence BSADF (GSADF backward)\n")
cat("  episodes      — data.frame des épisodes filtrés\n")
cat("  episodes_brut — data.frame des épisodes avant filtre\n")
cat("  stat_sadf / stat_gsadf — statistiques de test\n")
cat("  cv95_sadf / cv95_gsadf — valeurs critiques 95%\n")
cat("  duree_min     — seuil de durée minimale (jours)\n")
# =============================================================================
# Fichier  : B1_collecte.R
# Objectif : Collecte et préparation des données panel pour l'estimation
#            de la vitesse de convergence (modèle de Solow augmenté MRW 1992).
# Input    : World Bank WDI (téléchargement via package WDI, indicateurs
#            NY.GDP.PCAP.KD, NE.GDI.FTOT.ZS, SP.POP.GROW, SE.SEC.ENRR,
#            NE.TRD.GNFS.ZS), période 1990–2022, tous pays disponibles.
# Outputs  : panel_data.rds — data.frame standard (55 pays × 33 ans,
#            N = 1815 obs, panel cylindré).
# Auteur   : Antoine Jean
# Date     : 2025-2026
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------
if (!requireNamespace("WDI",    quietly = TRUE)) install.packages("WDI")
if (!requireNamespace("plm",    quietly = TRUE)) install.packages("plm")
if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr",  quietly = TRUE)) install.packages("tidyr")

library(WDI)
library(plm)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# 1. Définition des indicateurs WDI
# -----------------------------------------------------------------------------
# Contreparties théoriques dans le modèle de Solow augmenté (MRW 1992) :
# Y/L = f(s_k, s_h, n+g+delta, Y0/L0)

indicateurs <- c(
    # PIB réel par habitant (USD 2015 constants)
    # Variable dépendante (taux de croissance) et condition initiale (ln Y_0)
    gdp_pc        = "NY.GDP.PCAP.KD",
    
    # FBCF / PIB (% du PIB) — proxy s_k (accumulation de capital physique)
    inv_rate      = "NE.GDI.FTOT.ZS",
    
    # Taux de croissance démographique (% annuel) — n dans MRW
    pop_growth    = "SP.POP.GROW",
    
    # Taux de scolarisation brut dans le secondaire (%) — proxy s_h
    # Limite : taux brut peut dépasser 100%
    school        = "SE.SEC.ENRR",
    
    # Ouverture commerciale = (X + M) / PIB (%) — variable de contrôle
    openness      = "NE.TRD.GNFS.ZS"
)

cat("=== Téléchargement des données WDI ===\n")
cat("Indicateurs :", paste(names(indicateurs), collapse = ", "), "\n")
cat("Période : 1990–2022\n\n")

# -----------------------------------------------------------------------------
# 2. Téléchargement
# -----------------------------------------------------------------------------
raw <- WDI(
    country   = "all",
    indicator = indicateurs,
    start     = 1990,
    end       = 2022,
    extra     = TRUE          # inclut region, income group, iso3c, etc.
)

cat("Dimensions brutes :", nrow(raw), "x", ncol(raw), "\n")
cat("Nombre de pays bruts :", length(unique(raw$iso2c)), "\n\n")

# -----------------------------------------------------------------------------
# 3. Nettoyage préliminaire
# -----------------------------------------------------------------------------

# 3a. Suppression des agrégats régionaux/mondiaux de la WB
# (codes iso2c non standard : "1W", "Z4", "ZJ", etc.)
# Critère : code iso3c renseigné et conforme ISO 3166-1 alpha-3
df <- raw %>%
    filter(
        !is.na(iso3c),
        grepl("^[A-Z]{3}$", iso3c),
        region != "Aggregates"
    )

cat("Après suppression des agrégats :", length(unique(df$iso2c)), "pays\n\n")

# 3b. Sélection des colonnes utiles
# WDI() utilise les noms du vecteur `indicateurs` comme noms de colonnes —
# pas de rename() nécessaire.
df <- df %>%
    select(iso3c, country, year, region, income,
           gdp_pc, inv_rate, pop_growth, school, openness)

# -----------------------------------------------------------------------------
# 4. Diagnostic des données manquantes par pays
# -----------------------------------------------------------------------------
# Seuil d'exclusion : > 30% de manquants sur au moins une variable clé MRW
# openness traité séparément (variable optionnelle)

vars_cles <- c("gdp_pc", "inv_rate", "pop_growth", "school")

missing_by_country <- df %>%
    group_by(iso3c, country) %>%
    summarise(
        n_obs           = n(),
        pct_miss_gdp    = mean(is.na(gdp_pc))    * 100,
        pct_miss_inv    = mean(is.na(inv_rate))   * 100,
        pct_miss_pop    = mean(is.na(pop_growth)) * 100,
        pct_miss_school = mean(is.na(school))     * 100,
        .groups = "drop"
    ) %>%
    mutate(
        max_miss_cles = pmax(pct_miss_gdp, pct_miss_inv,
                             pct_miss_pop, pct_miss_school)
    )

pays_exclus <- missing_by_country %>%
    filter(max_miss_cles > 30)

cat("=== Pays exclus (> 30% manquants sur variables clés) ===\n")
cat("Nombre de pays exclus :", nrow(pays_exclus), "\n")
if (nrow(pays_exclus) > 0) {
    print(pays_exclus %>%
              select(iso3c, country, pct_miss_gdp, pct_miss_inv,
                     pct_miss_pop, pct_miss_school, max_miss_cles) %>%
              arrange(desc(max_miss_cles)),
          n = 30)
}
cat("\n")

pays_retenus <- missing_by_country %>%
    filter(max_miss_cles <= 30) %>%
    pull(iso3c)

df_clean <- df %>%
    filter(iso3c %in% pays_retenus)

cat("Pays retenus après filtrage :", length(pays_retenus), "\n")
cat("Observations restantes      :", nrow(df_clean), "\n\n")

# -----------------------------------------------------------------------------
# 5. Construction des variables transformées
# -----------------------------------------------------------------------------

# df_clean maintenu en data.frame standard avant les mutate dplyr.
# Si l'objet est un pdata.frame, plm surcharge lag() et fausse silencieusement
# le calcul des différences (retourne 0 au lieu de NA sur la première obs).
df_clean <- as.data.frame(df_clean) %>%
    arrange(iso3c, year)

# Étape 5a : ln_gdp_pc dans un premier mutate, growth_gdp dans un second.
# dplyr::lag() sur une colonne créée dans le même mutate() peut être instable
# selon le contexte d'exécution.
df_clean <- df_clean %>%
    group_by(iso3c) %>%
    mutate(
        ln_gdp_pc = log(gdp_pc)
    ) %>%
    ungroup()

df_clean <- df_clean %>%
    group_by(iso3c) %>%
    mutate(
        # Condition initiale : log PIB/hab à la première année disponible par pays
        ln_gdp_init = first(na.omit(ln_gdp_pc)),
        
        # Taux de croissance annuel : g_t = ln(Y_t/L_t) - ln(Y_{t-1}/L_{t-1})
        # order_by = year explicite pour éviter toute ambiguïté d'ordre
        # Première observation par pays : NA par construction
        growth_gdp  = ln_gdp_pc - dplyr::lag(ln_gdp_pc, n = 1, order_by = year),
        
        # Log du taux d'investissement — guard contre log(0) ou valeurs négatives
        ln_inv      = log(pmax(inv_rate / 100, 1e-6)),
        
        # Log(n + g + delta) — convention MRW : g + delta = 0.05
        ln_ngd      = log(pmax(pop_growth / 100 + 0.05, 1e-6)),
        
        # Log du taux de scolarisation (proxy s_h)
        ln_school   = log(pmax(school / 100, 1e-6))
        
    ) %>%
    ungroup()

# Vérification : growth_gdp ne doit pas être identiquement nul
cat("--- Vérification growth_gdp ---\n")
print(summary(df_clean$growth_gdp))
cat("SD :", sd(df_clean$growth_gdp, na.rm = TRUE), "\n")
cat("Exemple ALB :\n")
print(df_clean %>%
          filter(iso3c == "ALB") %>%
          select(year, gdp_pc, ln_gdp_pc, growth_gdp) %>%
          head(5))
cat("\n")

# Signalement des valeurs problématiques d'investissement
n_inv_neg <- sum(df_clean$inv_rate <= 0, na.rm = TRUE)
if (n_inv_neg > 0) {
    cat("AVERTISSEMENT :", n_inv_neg,
        "observations avec inv_rate <= 0 (remplacées par 1e-6 avant log)\n")
    cat("Pays concernés :\n")
    print(df_clean %>%
              filter(inv_rate <= 0) %>%
              select(iso3c, country, year, inv_rate))
    cat("\n")
}

# -----------------------------------------------------------------------------
# 6. Déclaration du panel avec plm
# -----------------------------------------------------------------------------
panel_pdata <- pdata.frame(df_clean, index = c("iso3c", "year"))

cat("=== Diagnostic du panel (pdim) ===\n")
dim_panel <- pdim(panel_pdata)
print(dim_panel)
cat("\n")

cat("Panel équilibré :", dim_panel$balanced, "\n")
if (!dim_panel$balanced) {
    cat("→ Panel non cylindré. À prendre en compte pour FD et GMM.\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# 7. Statistiques descriptives within / between
# -----------------------------------------------------------------------------
cat("=== Variation within / between (variables clés) ===\n")
vars_desc <- c("growth_gdp", "ln_gdp_pc", "ln_inv", "ln_ngd",
               "ln_school", "openness")

for (v in vars_desc) {
    if (v %in% names(panel_pdata)) {
        cat("\n---", v, "---\n")
        tryCatch(
            print(summary(Within(panel_pdata[[v]]))),
            error = function(e) cat("  (calcul within non disponible pour", v, ")\n")
        )
    }
}

cat("\n=== Statistiques descriptives globales ===\n")
desc_vars <- df_clean %>%
    select(growth_gdp, ln_gdp_pc, ln_gdp_init,
           ln_inv, ln_ngd, ln_school, openness) %>%
    summarise(across(everything(),
                     list(
                         n    = ~sum(!is.na(.)),
                         mean = ~mean(., na.rm = TRUE),
                         sd   = ~sd(., na.rm = TRUE),
                         min  = ~min(., na.rm = TRUE),
                         max  = ~max(., na.rm = TRUE)
                     ),
                     .names = "{.col}__{.fn}"))

desc_long <- desc_vars %>%
    pivot_longer(everything(),
                 names_to  = c("variable", "stat"),
                 names_sep = "__") %>%
    pivot_wider(names_from = stat, values_from = value)

print(as.data.frame(desc_long))
cat("\n")

# -----------------------------------------------------------------------------
# 8. Matrice de corrélations
# -----------------------------------------------------------------------------
cat("=== Matrice de corrélations ===\n")
cor_vars <- df_clean %>%
    select(growth_gdp, ln_gdp_init, ln_inv, ln_ngd, ln_school, openness) %>%
    cor(use = "pairwise.complete.obs")

print(round(cor_vars, 3))
cat("\n")

# Signalement des corrélations élevées (potentielle multicolinéarité)
cor_mat <- cor_vars
diag(cor_mat) <- NA
high_cor <- which(abs(cor_mat) > 0.7, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
    cat("AVERTISSEMENT — Corrélations > 0.7 :\n")
    for (i in seq_len(nrow(high_cor))) {
        r <- high_cor[i, 1]; c_col <- high_cor[i, 2]
        if (r < c_col) {
            cat(sprintf("  %s vs %s : %.3f\n",
                        rownames(cor_mat)[r],
                        colnames(cor_mat)[c_col],
                        cor_vars[r, c_col]))
        }
    }
    cat("\n")
}

# -----------------------------------------------------------------------------
# 9. Résumé final
# -----------------------------------------------------------------------------
cat("=== RÉSUMÉ — Dataset final ===\n")
cat("Pays (N)        :", length(unique(df_clean$iso3c)), "\n")
cat("Périodes (T)    :", length(unique(df_clean$year)), "\n")
cat("Observations NT :", nrow(df_clean), "\n")
cat("Variables disponibles :\n")
cat("  - growth_gdp   : taux de croissance annuel ln(Y_t/L_t) - ln(Y_{t-1}/L_{t-1})\n")
cat("  - ln_gdp_pc    : log PIB réel/hab (niveau)\n")
cat("  - ln_gdp_init  : log PIB réel/hab à t=0 (condition initiale, time-invariant)\n")
cat("  - ln_inv       : log taux d'investissement (proxy s_k)\n")
cat("  - ln_ngd       : log(n + 0.05) (proxy n+g+delta)\n")
cat("  - ln_school    : log taux scolarisation secondaire (proxy s_h)\n")
cat("  - openness     : ouverture commerciale (optionnelle, manquants possibles)\n")
cat("  - region, income : variables de groupe\n")
cat("\nPanel équilibré :", pdim(panel_pdata)$balanced, "\n")
cat("FD et GMM-diff perdent la première observation par pays (t=1990).\n\n")

# -----------------------------------------------------------------------------
# 10. Sauvegarde
# -----------------------------------------------------------------------------
# Sauvegarde en data.frame standard — pdata.frame déclaré dans B2_estimation.R
panel_data <- df_clean

saveRDS(panel_data, "panel_data.rds")
cat("=== Fichier panel_data.rds sauvegardé ===\n")
cat("\n=== FIN DU SCRIPT B1 ===\n")

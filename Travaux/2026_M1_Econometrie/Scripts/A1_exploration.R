# =============================================================================
# Fichier  : A1_exploration.R
# Objectif : Analyse descriptive complète de OOPs.dta en amont de toute
#            modélisation. Ce script ne produit aucune estimation de modèle.
# Input    : OOPs.dta
# Outputs  : figures/*.png + tableaux imprimés en console
# Auteur   : Antoine Jean
# Date     : 2025-2026
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES ET CONFIGURATION
# -----------------------------------------------------------------------------

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(corrplot)
library(car)
library(knitr)
library(scales)
library(patchwork)

# Création du dossier figures
if (!dir.exists("figures")) dir.create("figures")

# Seed pour reproductibilité
set.seed(42)

# Thème ggplot2 sobre pour export LaTeX
theme_set(theme_bw(base_size = 11))

# =============================================================================
# 1. CHARGEMENT ET INVENTAIRE DES VARIABLES
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Lecture du fichier
# -----------------------------------------------------------------------------

dta <- read_dta("OOPs.dta")

cat("=== DIMENSIONS DU DATASET ===\n")
cat("Nombre d'observations :", nrow(dta), "\n")
cat("Nombre de variables   :", ncol(dta), "\n\n")


# -----------------------------------------------------------------------------
# 1.2 Inventaire complet : label, type Stata, type R, valeurs manquantes
# -----------------------------------------------------------------------------

# Extraction des labels de variables (attribut Stata)
var_labels <- sapply(dta, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) "" else as.character(lab)
})

# Type Stata d'origine (stocké dans l'attribut format.stata)
var_stata_type <- sapply(dta, function(x) {
    ft <- attr(x, "format.stata")
    if (is.null(ft)) class(x)[1] else ft
})

# Type R effectif après lecture
var_r_class <- sapply(dta, function(x) class(x)[1])

# Nombre de valeurs manquantes
var_na <- sapply(dta, function(x) sum(is.na(x)))

# Proportion de manquants
var_na_pct <- round(var_na / nrow(dta) * 100, 2)

# Assemblage du tableau inventaire
inventaire <- data.frame(
    Variable     = names(dta),
    Label        = var_labels,
    Type_Stata   = var_stata_type,
    Type_R       = var_r_class,
    N_Manquants  = var_na,
    Pct_Manquant = var_na_pct,
    stringsAsFactors = FALSE
)

cat("=== INVENTAIRE DES VARIABLES ===\n")
print(kable(inventaire, row.names = FALSE, format = "pipe"))
cat("\n")

# Sauvegarde pour référence
write.csv(inventaire, "figures/inventaire_variables.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 1.3 Identification sans ambiguïté de oops_sante
# -----------------------------------------------------------------------------

cat("=== VARIABLE DÉPENDANTE : oops_sante ===\n")

if (!"oops_sante" %in% names(dta)) {
    stop("ERREUR : la variable 'oops_sante' est introuvable dans OOPs.dta.
       Vérifier l'orthographe exacte du nom de variable dans le fichier Stata.")
}

cat("Label       :", attr(dta$oops_sante, "label"), "\n")
cat("Type R      :", class(dta$oops_sante)[1], "\n")
cat("Format Stata:", attr(dta$oops_sante, "format.stata"), "\n")
cat("N manquants :", sum(is.na(dta$oops_sante)), "\n\n")


# -----------------------------------------------------------------------------
# 1.4 Sélection raisonnée des variables explicatives
# -----------------------------------------------------------------------------

# Catégories de déterminants :
#   [S] Socio-démographiques : âge, sexe, taille du ménage, statut matrimonial
#   [E] Économiques          : revenu/consommation du ménage, quintile de richesse
#   [G] Géographiques        : milieu (urbain/rural), région administrative
#   [H] Santé                : état de santé déclaré, maladie chronique,
#                              couverture maladie (assurance)

vars_candidates <- list(
    
    # [S] Socio-démographiques — caractéristiques du chef de ménage (CM)
    # hage    : âge du CM — déterminant central de la demande de soins (Grossman 1972)
    # hgender : genre du CM — proxy des inégalités de genre dans l'accès aux soins
    # hhsize  : taille du ménage — économies d'échelle et dilution du budget santé
    # hmstat  : situation matrimoniale du CM — structure de soutien social
    # heduc   : niveau d'éducation du CM — proxy de connaissance sanitaire et de
    #           revenu permanent (Grossman 1972 : l'éducation accroît l'efficacité
    #           de la production de santé)
    # halfab  : alphabétisation du CM — complément à heduc, taux de manquants souvent plus faible
    sociodem = c("hage", "hgender", "hhsize", "hmstat", "heduc", "halfab"),
    
    # [E] Économiques
    # pcexp : dépense per capita (indicateur de bien-être) — proxy standard du revenu
    #         permanent dans les LSMS ; préféré au revenu déclaré (sous-déclaration)
    # dtot  : consommation annuelle totale du ménage — alternative à pcexp en niveaux ménage
    # dnal  : consommation non-alimentaire — proxy de la capacité à payer hors alimentation
    economique = c("pcexp", "dtot", "dnal"),
    
    # [G] Géographiques
    # milieu     : urbain/rural — différentiel d'offre de soins et de prix
    # region     : région administrative (8 modalités) — hétérogénéité géographique
    # prefecture : granularité infra-régionale si la région est trop agrégée
    geographique = c("milieu", "region", "prefecture"),
    
    # [H] Santé et couverture
    # s03q01 : problème de santé au cours des 30 derniers jours — variable de besoin
    #          (need factor, Andersen 1995) ; déterminant de la participation aux soins
    # s03q05 : a consulté un service de santé — variable de recours effectif ;
    #          candidat naturel à l'équation de sélection dans le modèle de Heckman
    # s03q32 : couverture par une assurance maladie — réduit le prix effectif des soins ;
    #          effet sur oops ambigu (substitution vs complémentarité)
    # s03q19 : problème de santé au cours des 12 derniers mois — capte la morbidité
    #          sur un horizon plus long, approximation de la chronicité
    # hhandig : handicap majeur du CM — besoin de soins structurel
    sante = c("s03q01", "s03q05", "s03q32", "s03q19", "hhandig")
)

# Vérification de présence effective dans le dataset
cat("=== VÉRIFICATION DES VARIABLES CANDIDATES ===\n")
for (cat_name in names(vars_candidates)) {
    cat("\nCatégorie :", toupper(cat_name), "\n")
    for (v in vars_candidates[[cat_name]]) {
        present <- v %in% names(dta)
        cat(sprintf("  %-30s %s\n", v, ifelse(present, "[OK]", "[ABSENT — à corriger]")))
    }
}

# Construction de la liste effective des variables présentes
vars_presentes <- unlist(vars_candidates)
vars_presentes <- vars_presentes[vars_presentes %in% names(dta)]

cat("\nVariables explicatives retenues (présentes dans le dataset) :\n")
cat(paste(vars_presentes, collapse = ", "), "\n\n")


# =============================================================================
# 2. ANALYSE DE oops_sante
# =============================================================================

y <- dta$oops_sante
y_pos <- y[!is.na(y) & y > 0] # Sous-échantillon des dépenses strictement positives
y_all <- y[!is.na(y)] # Toutes les observations non manquantes


# -----------------------------------------------------------------------------
# 2.1 Statistiques descriptives de oops_sante
# -----------------------------------------------------------------------------

stats_y <- data.frame(
    Statistique = c("N total", "N manquants", "N non-manquants",
                    "Minimum", "Maximum", "Moyenne", "Médiane",
                    "Écart-type", "Skewness", "Kurtosis (excès)",
                    "Part des zéros (%)"),
    Valeur = c(
        length(y),
        sum(is.na(y)),
        sum(!is.na(y)),
        round(min(y_all), 2),
        round(max(y_all), 2),
        round(mean(y_all), 2),
        round(median(y_all), 2),
        round(sd(y_all), 2),
        round(skewness(y_all), 4),
        round(kurtosis(y_all) - 3, 4),   # Kurtosis excès (normale = 0)
        round(mean(y_all == 0) * 100, 2)
    )
)

cat("=== STATISTIQUES DESCRIPTIVES DE oops_sante ===\n")
print(kable(stats_y, row.names = FALSE, format = "pipe"))
cat("\n")

# Part des zéros — valeur clé pour le choix du modèle
pct_zeros <- mean(y_all == 0) * 100
cat(sprintf("Part des zéros : %.2f%%\n", pct_zeros))
cat("→ Implication pour la modélisation : voir section commentaires.\n\n")


# -----------------------------------------------------------------------------
# 2.2 Histogrammes
# -----------------------------------------------------------------------------

df_y <- data.frame(oops_sante = y_all)
df_y_pos <- data.frame(oops_sante = y_pos)

# --- Histogramme global (incluant les zéros) ---
p_hist_global <- ggplot(df_y, aes(x = oops_sante)) +
    geom_histogram(bins = 60, fill = "#2c7bb6", color = "white", alpha = 0.85) +
    coord_cartesian(xlim = c(0, 1500000)) +
    labs(
        title = "Distribution de oops\\_sante (échantillon complet)",
        subtitle = sprintf("N = %d | Part des zéros = %.1f%%", length(y_all), pct_zeros),
        x = "Dépense de santé out-of-pocket (GNF)",
        y = "Fréquence"
    )

ggsave("figures/hist_oops_global.png", p_hist_global,
       width = 7, height = 4, dpi = 150)
cat("Figure sauvegardée : figures/hist_oops_global.png\n")

# --- Histogramme conditionnel sur oops_sante > 0 ---
p_hist_pos <- ggplot(df_y_pos, aes(x = oops_sante)) +
    geom_histogram(bins = 50, fill = "#d7191c", color = "white", alpha = 0.85) +
    scale_x_continuous(labels = comma) +
    labs(
        title = "Distribution de oops\\_sante conditionnelle (oops\\_sante > 0)",
        subtitle = sprintf("N = %d observations avec dépense positive", length(y_pos)),
        x = "Dépense de santé out-of-pocket (GNF)",
        y = "Fréquence"
    )

ggsave("figures/hist_oops_positif.png", p_hist_pos,
       width = 7, height = 4, dpi = 150)
cat("Figure sauvegardée : figures/hist_oops_positif.png\n")

# --- Histogramme de log(oops_sante) sur les positifs ---
# La log-transformation est standard pour les dépenses de santé à queue épaisse
df_y_pos$log_oops <- log(df_y_pos$oops_sante)

p_hist_log <- ggplot(df_y_pos, aes(x = log_oops)) +
    geom_histogram(bins = 40, fill = "#1a9641", color = "white", alpha = 0.85) +
    labs(
        title = "Distribution de log(oops\\_sante) conditionnelle (oops\\_sante > 0)",
        x = "log(Dépense de santé out-of-pocket)",
        y = "Fréquence"
    )

ggsave("figures/hist_log_oops_positif.png", p_hist_log,
       width = 7, height = 4, dpi = 150)
cat("Figure sauvegardée : figures/hist_log_oops_positif.png\n\n")


# -----------------------------------------------------------------------------
# 2.3 Test de normalité (Shapiro-Wilk)
# -----------------------------------------------------------------------------
# Limite : Shapiro-Wilk n'est fiable que pour n <= 5000.
# Au-delà, la puissance est telle que H0 est rejetée mécaniquement.
# On applique le test sur un sous-échantillon aléatoire si nécessaire.

cat("=== TEST DE NORMALITÉ (Shapiro-Wilk) ===\n")

sw_test_global <- function(x, label, n_max = 4999) {
    x_clean <- x[!is.na(x)]
    n <- length(x_clean)
    if (n > n_max) {
        cat(sprintf("[%s] N = %d > %d → sous-échantillon aléatoire utilisé.\n",
                    label, n, n_max))
        x_clean <- sample(x_clean, n_max)
    }
    result <- shapiro.test(x_clean)
    cat(sprintf("[%s] W = %.6f | p-value = %.4e\n", label, result$statistic, result$p.value))
    return(result)
}

sw_all <- sw_test_global(y_all, "oops_sante (complet)")
sw_pos <- sw_test_global(y_pos, "oops_sante > 0")
sw_log <- sw_test_global(log(y_pos), "log(oops_sante) | > 0")

cat("\n")
cat("Interprétation indicative :\n")
cat("  - Rejet de H0 (normalité) attendu pour y brut et y|y>0 (queue épaisse, asymétrie).\n")
cat("  - Si log(oops_sante)|>0 se rapproche de la normalité, cela oriente vers\n")
cat("    un modèle two-part avec régression log-normale sur l'équation de niveau.\n")
cat("  - L'absence de normalité en soi ne disqualifie pas l'OLS (TCL),\n")
cat("    mais la forte asymétrie et les zéros excessifs motivent les alternatives.\n\n")


# -----------------------------------------------------------------------------
# 2.4 Commentaires sur les implications pour la modélisation
# (hypothèses de travail, non des conclusions définitives)
# -----------------------------------------------------------------------------

cat("=== HYPOTHÈSES DE TRAVAIL POUR LES AGENTS A2/A3 ===\n")
cat(sprintf("Part des zéros : %.2f%%\n", pct_zeros))
cat("
Hypothèse H1 (censure) :
  Les zéros reflètent une dépense latente non observable (contrainte budgétaire,
  absence d'offre de soins). → Modèle Tobit type I cohérent sous cette hypothèse.
  → À invalider si des ménages 'en bonne santé' sont structurellement non-consommateurs.

Hypothèse H2 (non-participation) :
  Les zéros résultent d'une décision de non-recours aux soins (pas de besoin,
  préférence, barrières culturelles). → Modèle de sélection de Heckman ou
  modèle two-part (Cragg) plus appropriés sous cette hypothèse.
  → La discrimination entre H1 et H2 relève de la connaissance du terrain
    (Guinée 2018-19) et des tests d'adéquation : rôle de l'agent A3.

Hypothèse H3 (queue épaisse) :
  L'asymétrie positive de la distribution des dépenses positives suggère
  une log-transformation ou un estimateur robuste aux valeurs extrêmes.
  → À confirmer par l'analyse des outliers (section 4).
")


# =============================================================================
# 3. ANALYSE DES VARIABLES EXPLICATIVES RETENUES
# =============================================================================
# Note : cette section adapte son comportement selon que les variables
# sont numériques continues ou catégorielles (factor/haven_labelled).


# -----------------------------------------------------------------------------
# 3.1 Statistiques descriptives standards
# -----------------------------------------------------------------------------

cat("=== STATISTIQUES DESCRIPTIVES — VARIABLES EXPLICATIVES ===\n\n")

# Distinction variables continues vs catégorielles
# Les variables haven_labelled avec peu de modalités sont traitées comme catégorielles
is_categorielle <- function(x) {
    inherits(x, c("factor", "haven_labelled")) && length(unique(x[!is.na(x)])) <= 10
}

vars_continues <- vars_presentes[
    sapply(dta[vars_presentes], function(x) is.numeric(x) && !is_categorielle(x))
]
vars_categorielles <- vars_presentes[
    sapply(dta[vars_presentes], is_categorielle)
]

cat("Variables continues identifiées :\n", paste(vars_continues, collapse = ", "), "\n\n")
cat("Variables catégorielles identifiées :\n", paste(vars_categorielles, collapse = ", "), "\n\n")

# Statistiques pour variables continues — base R pur (compatible toutes versions dplyr)
if (length(vars_continues) > 0) {
    df_cont <- as.data.frame(dta)[, vars_continues, drop = FALSE]
    stats_continues <- do.call(rbind, lapply(vars_continues, function(v) {
        x <- as.numeric(df_cont[[v]])
        data.frame(
            Variable = v,
            N        = sum(!is.na(x)),
            Moy      = round(mean(x, na.rm = TRUE), 3),
            Sd       = round(sd(x,   na.rm = TRUE), 3),
            Min      = round(min(x,  na.rm = TRUE), 3),
            Med      = round(median(x, na.rm = TRUE), 3),
            Max      = round(max(x,  na.rm = TRUE), 3),
            NA_pct   = round(mean(is.na(x)) * 100, 1),
            stringsAsFactors = FALSE
        )
    }))
    
    cat("--- Statistiques descriptives — Variables continues ---\n")
    print(kable(stats_continues, row.names = FALSE, format = "pipe"))
    cat("\n")
}


# -----------------------------------------------------------------------------
# 3.2 Tableaux de fréquences — variables catégorielles
# -----------------------------------------------------------------------------

if (length(vars_categorielles) > 0) {
    cat("--- Tableaux de fréquences — Variables catégorielles ---\n\n")
    for (v in vars_categorielles) {
        x <- dta[[v]]
        # Conversion en factor pour tabulation
        if (inherits(x, "haven_labelled")) {
            x <- as_factor(x)
        }
        tab <- as.data.frame(table(x, useNA = "ifany"))
        tab$Pct <- round(tab$Freq / sum(tab$Freq) * 100, 2)
        names(tab) <- c(v, "Effectif", "Pourcentage (%)")
        cat(sprintf("Variable : %s\n", v))
        print(kable(tab, row.names = FALSE, format = "pipe"))
        cat("\n")
    }
}


# -----------------------------------------------------------------------------
# 3.3 Matrice de corrélation (variables continues uniquement)
# -----------------------------------------------------------------------------

if (length(vars_continues) >= 2) {
    cat("=== MATRICE DE CORRÉLATION (Pearson) ===\n")
    
    # Inclure oops_sante dans la matrice
    vars_corr <- c("oops_sante", vars_continues)
    vars_corr <- vars_corr[vars_corr %in% names(dta)]
    
    df_corr <- dta %>%
        .[, vars_corr] %>%
        mutate(across(everything(), as.numeric)) %>%
        na.omit()
    
    mat_corr <- cor(df_corr, method = "pearson")
    
    cat(sprintf("Matrice calculée sur %d observations complètes.\n\n", nrow(df_corr)))
    print(round(mat_corr, 3))
    
    # Visualisation
    png("figures/matrice_correlation.png", width = 800, height = 700, res = 120)
    corrplot(mat_corr,
             method = "color",
             type = "upper",
             tl.cex = 0.75,
             addCoef.col = "black",
             number.cex = 0.6,
             col = colorRampPalette(c("#d73027", "white", "#1a9641"))(200),
             title = "Matrice de corrélation (Pearson)",
             mar = c(0, 0, 2, 0))
    dev.off()
    cat("Figure sauvegardée : figures/matrice_correlation.png\n\n")
}


# -----------------------------------------------------------------------------
# 3.4 Variance Inflation Factor (VIF) — détection de multicolinéarité
# -----------------------------------------------------------------------------
# Le VIF est calculé via une régression auxiliaire OLS incluant toutes
# les variables continues. Un VIF > 10 est le seuil standard d'alerte ;
# certains auteurs retiennent 5 comme seuil conservateur.
# ATTENTION : ceci N'EST PAS une estimation de modèle finale — c'est
# un diagnostic purement descriptif de la structure des régresseurs.

if (length(vars_continues) >= 2) {
    cat("=== VIF — DIAGNOSTIC DE MULTICOLINÉARITÉ ===\n")
    
    df_vif <- dta %>%
        .[, c('oops_sante', vars_continues)] %>%
        mutate(across(everything(), as.numeric)) %>%
        na.omit()
    
    # Formule OLS auxiliaire pour le VIF uniquement
    formule_vif <- as.formula(
        paste("oops_sante ~", paste(vars_continues, collapse = " + "))
    )
    
    tryCatch({
        mod_vif <- lm(formule_vif, data = df_vif)
        vif_vals <- vif(mod_vif)
        df_vif_out <- data.frame(
            Variable = names(vif_vals),
            VIF      = round(vif_vals, 3),
            Alerte   = ifelse(vif_vals > 10, "CRITIQUE (>10)",
                              ifelse(vif_vals > 5, "Modéré (>5)", "OK"))
        )
        print(kable(df_vif_out, row.names = FALSE, format = "pipe"))
        cat("\n")
        cat("Seuil d'alerte : VIF > 10 (critique), VIF > 5 (conservateur).\n")
        cat("En présence de multicolinéarité forte, envisager :\n")
        cat("  - Exclusion de la variable redondante\n")
        cat("  - Construction d'un indice composite (ex. score de richesse)\n\n")
    }, error = function(e) {
        cat("ERREUR dans le calcul VIF :", conditionMessage(e), "\n")
        cat("Vérifier que les variables continues sont bien numériques et sans colinéarité parfaite.\n\n")
    })
}


# =============================================================================
# 4. DÉTECTION DES VALEURS ABERRANTES SUR oops_sante
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1 Méthode IQR
# -----------------------------------------------------------------------------

cat("=== DÉTECTION DES OUTLIERS — MÉTHODE IQR ===\n")

q1  <- quantile(y_all, 0.25, na.rm = TRUE)
q3  <- quantile(y_all, 0.75, na.rm = TRUE)
iqr <- q3 - q1

borne_inf <- q1 - 1.5 * iqr
borne_sup <- q3 + 1.5 * iqr

n_outliers_iqr <- sum(y_all < borne_inf | y_all > borne_sup, na.rm = TRUE)
pct_outliers_iqr <- round(n_outliers_iqr / length(y_all) * 100, 2)

cat(sprintf("Q1          : %.2f\n", q1))
cat(sprintf("Q3          : %.2f\n", q3))
cat(sprintf("IQR         : %.2f\n", iqr))
cat(sprintf("Borne inf   : %.2f\n", borne_inf))
cat(sprintf("Borne sup   : %.2f\n", borne_sup))
cat(sprintf("N outliers  : %d (%.2f%%)\n\n", n_outliers_iqr, pct_outliers_iqr))


# -----------------------------------------------------------------------------
# 4.2 Méthode z-score
# -----------------------------------------------------------------------------

cat("=== DÉTECTION DES OUTLIERS — MÉTHODE Z-SCORE ===\n")
# Seuil standard : |z| > 3

z_scores <- (y_all - mean(y_all, na.rm = TRUE)) / sd(y_all, na.rm = TRUE)
n_outliers_z <- sum(abs(z_scores) > 3, na.rm = TRUE)
pct_outliers_z <- round(n_outliers_z / length(y_all) * 100, 2)

cat(sprintf("N outliers (|z| > 3) : %d (%.2f%%)\n\n", n_outliers_z, pct_outliers_z))


# -----------------------------------------------------------------------------
# 4.3 Boxplot de oops_sante avec identification des outliers
# -----------------------------------------------------------------------------

df_box <- data.frame(
    oops_sante = y_all,
    is_outlier = (y_all < borne_inf | y_all > borne_sup)
)

p_boxplot <- ggplot(df_box, aes(x = "", y = oops_sante)) +
    geom_boxplot(outlier.shape = NA, fill = "#a6cee3", width = 0.4) +
    geom_jitter(data = df_box[df_box$is_outlier, ],
                aes(y = oops_sante), color = "#d73027",
                width = 0.1, alpha = 0.5, size = 1) +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Boxplot de oops\\_sante — outliers IQR en rouge",
        x = "",
        y = "Dépense de santé out-of-pocket (GNF)"
    )

ggsave("figures/boxplot_oops_outliers.png", p_boxplot,
       width = 5, height = 5, dpi = 150)
cat("Figure sauvegardée : figures/boxplot_oops_outliers.png\n\n")


# -----------------------------------------------------------------------------
# 4.4 Recommandations sur le traitement des outliers
# -----------------------------------------------------------------------------

cat("=== RECOMMANDATIONS — TRAITEMENT DES VALEURS ABERRANTES ===\n")

# Rapport asymétrie post-log
skew_brut <- skewness(y_all)
skew_log  <- skewness(log(y_pos))

cat(sprintf("Skewness brut              : %.4f\n", skew_brut))
cat(sprintf("Skewness log(oops)|>0      : %.4f\n", skew_log))
cat(sprintf("Réduction d'asymétrie (log): %.1f%%\n\n",
            (1 - abs(skew_log) / abs(skew_brut)) * 100))

cat("Seuils de décision indicatifs :\n")
cat("  - Si part des outliers IQR > 5% ET asymétrie forte (|skew| > 2) :\n")
cat("    → log-transformation recommandée sur l'équation de niveau (two-part, Heckman).\n")
cat("  - Si outliers isolés extrêmes (< 1%) avec valeurs très élevées :\n")
cat("    → Winsorisation au 99e percentile envisageable,\n")
cat("      MAIS toujours reporter la sensibilité des résultats.\n")
cat("  - En aucun cas supprimer des observations sans justification substantielle.\n")
cat("  - Toute transformation doit être documentée et motivée dans le rapport.\n\n")

# Valeur du 99e percentile (référence pour winsorisation éventuelle)
p99 <- quantile(y_all, 0.99, na.rm = TRUE)
cat(sprintf("99e percentile de oops_sante : %.2f\n", p99))
cat("(valeur de référence si winsorisation retenue par l'agent A2)\n\n")


# =============================================================================
# 5. EXPORT — RÉCAPITULATIF DES FICHIERS PRODUITS
# =============================================================================

cat("=== FICHIERS PRODUITS PAR CE SCRIPT ===\n")
cat("figures/inventaire_variables.csv    — inventaire complet des variables\n")
cat("figures/hist_oops_global.png        — histogramme global de oops_sante\n")
cat("figures/hist_oops_positif.png       — histogramme conditionnel (>0)\n")
cat("figures/hist_log_oops_positif.png   — histogramme de log(oops_sante)|>0\n")
cat("figures/matrice_correlation.png     — matrice de corrélation Pearson\n")
cat("figures/boxplot_oops_outliers.png   — boxplot avec outliers IQR identifiés\n\n")

cat("--- Fin du script exploration_A1.R ---\n")

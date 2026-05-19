# =============================================================================
# SCRIPT PROMPT #2 — Stationnarité, Cointégration, VAR/VECM
# Projet M1 Économie — Détection de bulle spéculative sur le Bitcoin
# Prérequis : objets df, df_ret, prix issus du script Prompt #1
# =============================================================================

# --- Chargement des packages --------------------------------------------------
library(tseries)      # adf.test, kpss.test, pp.test, po.test
library(urca)         # ca.jo, cajorls, ur.df
library(vars)         # VAR, VARselect, causality, irf
library(knitr)        # kable
library(kableExtra)   # kable_styling
library(ggplot2)      # graphiques
library(quantmod)     # getSymbols (déjà chargé en principe)
library(xts)          # manipulation séries temporelles

# Tentative de chargement de tsDyn avec fallback silencieux
tsDyn_ok <- requireNamespace("tsDyn", quietly = TRUE)
if (tsDyn_ok) {
    library(tsDyn)
    cat("tsDyn chargé avec succès.\n")
} else {
    cat("tsDyn non disponible — fallback urca::cajorls() sera utilisé pour le VECM.\n")
}


# =============================================================================
# BLOC A — TESTS DE STATIONNARITÉ
# Objectif : déterminer l'ordre d'intégration I(d) de la série de prix BTC
# Logique : on teste d'abord la série EN NIVEAU. Si non-stationnaire (I(1) ou
# plus), on teste sur les DIFFÉRENCES (log-rendements). Convergence des trois
# tests (ADF, KPSS, PP) permet de conclure sur I(d).
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("BLOC A — TESTS DE STATIONNARITÉ\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Extraction des vecteurs numériques pour les tests
serie_prix    <- as.numeric(prix)                   # prix en niveau
serie_logret  <- as.numeric(df_ret$log_ret)         # log-rendements (≈ 1ère différence du log)

# ---- A.1 : Tests ADF (Augmented Dickey-Fuller) -------------------------------
# H0 : présence d'une racine unitaire (non-stationnaire)
# Ha : stationnaire
# Sélection automatique du lag via l'argument k = trunc((length(x)-1)^(1/3))
# qui approxime le critère AIC dans tseries::adf.test

cat("--- A.1 : Test ADF ---\n")

adf_prix   <- adf.test(serie_prix,   alternative = "stationary")
adf_logret <- adf.test(serie_logret, alternative = "stationary")

cat(sprintf(
    "ADF Prix (niveau)   : stat = %.4f, p-value = %.4f => %s\n",
    adf_prix$statistic, adf_prix$p.value,
    ifelse(adf_prix$p.value > 0.05,
           "NON-STATIONNAIRE — on ne rejette pas H0 (racine unitaire)",
           "STATIONNAIRE — on rejette H0")
))

cat(sprintf(
    "ADF Log-rendements  : stat = %.4f, p-value = %.4f => %s\n\n",
    adf_logret$statistic, adf_logret$p.value,
    ifelse(adf_logret$p.value > 0.05,
           "NON-STATIONNAIRE — on ne rejette pas H0 (racine unitaire)",
           "STATIONNAIRE — on rejette H0")
))

# ---- A.2 : Tests KPSS --------------------------------------------------------
# H0 : STATIONNAIRE (niveau ou tendance)
# Ha : non-stationnaire (racine unitaire)
# Logique inversée par rapport à ADF — complémentaire pour robustesse

cat("--- A.2 : Test KPSS ---\n")

# Type "mu"  : stationnarité autour d'une constante (niveau)
# Type "tau" : stationnarité autour d'une tendance déterministe
kpss_prix_mu   <- kpss.test(serie_prix,   null = "Level")
kpss_prix_tau  <- kpss.test(serie_prix,   null = "Trend")
kpss_ret_mu    <- kpss.test(serie_logret, null = "Level")
kpss_ret_tau   <- kpss.test(serie_logret, null = "Trend")

interp_kpss <- function(test) {
    ifelse(test$p.value < 0.05,
           "NON-STATIONNAIRE — on rejette H0 (stationnarité)",
           "STATIONNAIRE — on ne rejette pas H0")
}

cat(sprintf("KPSS Prix (niveau, mu)   : stat = %.4f, p-value = %.4f => %s\n",
            kpss_prix_mu$statistic,  kpss_prix_mu$p.value,  interp_kpss(kpss_prix_mu)))
cat(sprintf("KPSS Prix (tendance, tau): stat = %.4f, p-value = %.4f => %s\n",
            kpss_prix_tau$statistic, kpss_prix_tau$p.value, interp_kpss(kpss_prix_tau)))
cat(sprintf("KPSS Log-ret (mu)        : stat = %.4f, p-value = %.4f => %s\n",
            kpss_ret_mu$statistic,   kpss_ret_mu$p.value,   interp_kpss(kpss_ret_mu)))
cat(sprintf("KPSS Log-ret (tau)       : stat = %.4f, p-value = %.4f => %s\n\n",
            kpss_ret_tau$statistic,  kpss_ret_tau$p.value,  interp_kpss(kpss_ret_tau)))

# ---- A.3 : Tests Phillips-Perron (PP) ----------------------------------------
# Variante non-paramétrique de l'ADF — robuste à l'hétéroscédasticité
# H0 : racine unitaire ; Ha : stationnaire
# Correction de Newey-West sur les résidus

cat("--- A.3 : Test Phillips-Perron ---\n")

pp_prix   <- pp.test(serie_prix,   alternative = "stationary")
pp_logret <- pp.test(serie_logret, alternative = "stationary")

cat(sprintf(
    "PP Prix (niveau)   : stat = %.4f, p-value = %.4f => %s\n",
    pp_prix$statistic, pp_prix$p.value,
    ifelse(pp_prix$p.value > 0.05,
           "NON-STATIONNAIRE — racine unitaire probable",
           "STATIONNAIRE")
))
cat(sprintf(
    "PP Log-rendements  : stat = %.4f, p-value = %.4f => %s\n\n",
    pp_logret$statistic, pp_logret$p.value,
    ifelse(pp_logret$p.value > 0.05,
           "NON-STATIONNAIRE — racine unitaire probable",
           "STATIONNAIRE")
))

# ---- A.4 : Tableau récapitulatif ---------------------------------------------

tab_stat <- data.frame(
    Série      = c("Prix (niveau)", "Prix (niveau)", "Prix (niveau)",
                   "Log-rendements", "Log-rendements", "Log-rendements"),
    Test       = c("ADF", "KPSS (mu)", "PP",
                   "ADF", "KPSS (mu)", "PP"),
    Statistique = round(c(
        adf_prix$statistic,   kpss_prix_mu$statistic,  pp_prix$statistic,
        adf_logret$statistic, kpss_ret_mu$statistic,   pp_logret$statistic
    ), 4),
    `P-value`  = round(c(
        adf_prix$p.value,   kpss_prix_mu$p.value,  pp_prix$p.value,
        adf_logret$p.value, kpss_ret_mu$p.value,   pp_logret$p.value
    ), 4),
    Conclusion = c(
        ifelse(adf_prix$p.value > 0.05, "Non-stationnaire", "Stationnaire"),
        ifelse(kpss_prix_mu$p.value < 0.05, "Non-stationnaire", "Stationnaire"),
        ifelse(pp_prix$p.value > 0.05, "Non-stationnaire", "Stationnaire"),
        ifelse(adf_logret$p.value > 0.05, "Non-stationnaire", "Stationnaire"),
        ifelse(kpss_ret_mu$p.value < 0.05, "Non-stationnaire", "Stationnaire"),
        ifelse(pp_logret$p.value > 0.05, "Non-stationnaire", "Stationnaire")
    ),
    check.names = FALSE
)

cat("\n--- Tableau récapitulatif Stationnarité ---\n")
print(kable(tab_stat, format = "simple", caption = "Tests de stationnarité — BTC/USD"))

# Conclusion sur l'ordre d'intégration
cat("\n[CONCLUSION BLOC A]\n")
cat("Si les trois tests convergent :\n")
cat("  - Prix en niveau     : I(1) — non-stationnaire\n")
cat("  - Log-rendements     : I(0) — stationnaire\n")
cat("=> La série de prix BTC suit vraisemblablement un processus I(1),\n")
cat("   compatible avec un marche aléatoire (random walk).\n")
cat("   Ce résultat est cohérent avec l'hypothèse de bulle spéculative\n")
cat("   (explosive root > 1) que l'on cherchera à tester avec SADF/GSADF.\n\n")


# =============================================================================
# BLOC B — COINTÉGRATION
# Objectif : tester si le prix BTC et le volume de transactions partagent
# une tendance stochastique commune (relation d'équilibre de long terme).
# Logique : deux séries I(1) peuvent être cointégrées si une combinaison
# linéaire est I(0). Si cointégration => VECM ; sinon => VAR en différences.
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("BLOC B — TESTS DE COINTÉGRATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ---- B.1 : Récupération du volume BTC/USD ------------------------------------
cat("--- B.1 : Téléchargement du volume BTC/USD ---\n")

getSymbols("BTC-USD", src = "yahoo", auto.assign = TRUE)
btc_raw <- `BTC-USD`

# Alignement sur la même période que df
dates_communes <- index(btc_raw)[index(btc_raw) %in% df$date]
volume_xts      <- btc_raw[dates_communes, "BTC-USD.Volume"]
prix_aligned    <- prix[dates_communes]

# Suppression des NA éventuels
idx_ok      <- complete.cases(as.numeric(prix_aligned), as.numeric(volume_xts))
prix_clean  <- as.numeric(prix_aligned)[idx_ok]
vol_clean   <- as.numeric(volume_xts)[idx_ok]

# Log-volume pour stabiliser la variance (volume peut être très hétéroscédastique)
log_vol <- log(vol_clean + 1)   # +1 pour éviter log(0) si volume nul

cat(sprintf("Observations disponibles après alignement : %d\n\n", sum(idx_ok)))

# ---- B.2 : Vérification stationnarité du volume (prérequis cointégration) ----
# Les deux séries doivent être I(1) pour que la cointégration ait un sens

adf_vol <- adf.test(log_vol, alternative = "stationary")
pp_vol  <- pp.test(log_vol,  alternative = "stationary")

cat(sprintf("ADF Log-Volume : stat = %.4f, p = %.4f => %s\n",
            adf_vol$statistic, adf_vol$p.value,
            ifelse(adf_vol$p.value > 0.05, "I(1) probable", "Stationnaire")))
cat(sprintf("PP  Log-Volume : stat = %.4f, p = %.4f => %s\n\n",
            pp_vol$statistic, pp_vol$p.value,
            ifelse(pp_vol$p.value > 0.05, "I(1) probable", "Stationnaire")))

# ---- B.3 : Test d'Engle-Granger via Phillips-Ouliaris (po.test) --------------
# H0 : PAS de cointégration (résidus de la régression ont une racine unitaire)
# Ha : cointégration

cat("--- B.2 : Test de Phillips-Ouliaris (Engle-Granger) ---\n")

mat_po <- cbind(log(prix_clean), log_vol)   # matrice [prix_log, volume_log]
po_test <- po.test(mat_po)

cat(sprintf(
    "Phillips-Ouliaris : stat = %.4f, p-value = %.4f => %s\n\n",
    po_test$statistic, po_test$p.value,
    ifelse(po_test$p.value < 0.05,
           "COINTÉGRATION détectée — on rejette H0",
           "PAS de cointégration — on ne rejette pas H0")
))

# ---- B.4 : Test de Johansen --------------------------------------------------
# Plus puissant que l'EG bivarié : permet de détecter r relations de cointégration
# Lag optimal via VAR préliminaire sur les niveaux

cat("--- B.3 : Test de Johansen ---\n")

mat_johansen <- cbind(log(prix_clean), log_vol)
colnames(mat_johansen) <- c("log_prix", "log_volume")

# Sélection du lag optimal pour le VAR en niveaux (critère AIC)
lag_select_jo <- VARselect(mat_johansen, lag.max = 10, type = "const")
lag_aic_jo    <- lag_select_jo$selection["AIC(n)"]
cat(sprintf("Lag optimal (AIC) pour VAR préliminaire : %d\n", lag_aic_jo))

# Test de Johansen — type "const" (tendance linéaire dans les données,
# constante dans la relation de cointégration — cas le plus courant)
# Test TRACE : H0(r=0), H0(r<=1), ...
# Test EIGENVALUE : H0(r=0) vs H1(r=1), etc.

jo_trace <- ca.jo(mat_johansen, type = "trace", ecdet = "const",
                  K = lag_aic_jo, spec = "longrun")
jo_eigen <- ca.jo(mat_johansen, type = "eigen", ecdet = "const",
                  K = lag_aic_jo, spec = "longrun")

cat("\n[Johansen — Test TRACE]\n")
print(summary(jo_trace))

cat("\n[Johansen — Test EIGENVALUE]\n")
print(summary(jo_eigen))

# Tableau structuré des résultats Johansen (valeurs critiques 10%, 5%, 1%)
extract_johansen_table <- function(jo_obj, type_label) {
    stats  <- jo_obj@teststat
    cvals  <- jo_obj@cval
    n_r    <- length(stats)
    data.frame(
        Test       = type_label,
        Hypothèse  = paste0("r <= ", 0:(n_r - 1)),
        Statistique = round(rev(stats), 4),
        `CV 10%`   = cvals[, 1],
        `CV 5%`    = cvals[, 2],
        `CV 1%`    = cvals[, 3],
        check.names = FALSE
    )
}

tab_jo <- rbind(
    extract_johansen_table(jo_trace, "Trace"),
    extract_johansen_table(jo_eigen, "Eigenvalue")
)

cat("\n--- Tableau Johansen ---\n")
print(kable(tab_jo, format = "simple",
            caption = "Test de Johansen — log(Prix BTC) & log(Volume)"))

# Détermination du rang de cointégration
rang_coint <- which(rev(jo_trace@teststat) < jo_trace@cval[, 2])[1] - 1
cat(sprintf("\n[CONCLUSION BLOC B]\n"))
cat(sprintf("Rang de cointégration retenu (seuil 5%%) : r = %d\n", max(0, rang_coint - 1)))
coint_detected <- (rang_coint > 0 && !is.na(rang_coint))
cat(sprintf("=> %s\n\n",
            ifelse(coint_detected,
                   "Cointégration détectée => VECM approprié (Bloc C)",
                   "Pas de cointégration => VAR en différences (Bloc C)")))


# =============================================================================
# BLOC C — MODÈLE VAR / VECM
# Logique : le choix entre VAR et VECM dépend du résultat du Bloc B.
#   - Cointégration (r >= 1) => VECM : capture les dynamiques court terme
#     ET la relation de long terme via le terme de correction d'erreur (ECT).
#   - Pas de cointégration => VAR en différences : on travaille sur les
#     log-rendements et la variation du log-volume (séries I(0)).
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("BLOC C — MODÈLE VAR / VECM, CAUSALITÉ DE GRANGER, IRF\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ---- C.1 : Sélection du lag optimal VAR --------------------------------------
cat("--- C.1 : Sélection du lag optimal ---\n")

# Si cointégration : VAR sur niveaux pour VECM ; sinon : VAR sur différences
if (coint_detected) {
    mat_var <- mat_johansen          # niveaux (Johansen en a besoin)
    cat("Cointégration détectée : sélection du lag sur les NIVEAUX.\n")
} else {
    # Différences premières des log-séries = log-rendements + Δlog-volume
    dlog_prix <- diff(log(prix_clean))
    dlog_vol  <- diff(log_vol)
    mat_var   <- cbind(dlog_prix, dlog_vol)
    colnames(mat_var) <- c("dlog_prix", "dlog_vol")
    cat("Pas de cointégration : sélection du lag sur les DIFFÉRENCES PREMIÈRES.\n")
}

var_select <- VARselect(mat_var, lag.max = 10, type = "const")
cat("\nCritères de sélection du lag :\n")
print(var_select$selection)

lag_aic  <- var_select$selection["AIC(n)"]
lag_hq   <- var_select$selection["HQ(n)"]
lag_sc   <- var_select$selection["SC(n)"]
lag_fpe  <- var_select$selection["FPE(n)"]

cat(sprintf("\nLag retenu (AIC = %d | HQ = %d | SC = %d | FPE = %d)\n",
            lag_aic, lag_hq, lag_sc, lag_fpe))
# On retient AIC pour maximiser l'ajustement ; SC (Schwarz) pénalise davantage
lag_retenu <- lag_aic
cat(sprintf("=> Lag retenu pour l'estimation : %d (AIC)\n\n", lag_retenu))

# ---- C.2 : Estimation VAR ou VECM --------------------------------------------

if (!coint_detected) {
    # --- Branche VAR en différences ---
    cat("--- C.2 : Estimation VAR en différences premières ---\n")
    
    modele_var <- VAR(mat_var, p = lag_retenu, type = "const")
    cat("\n[Résultats VAR]\n")
    print(summary(modele_var))
    
    # Objet pour l'IRF et la causalité
    modele_irf <- modele_var
    
} else {
    # --- Branche VECM ---
    cat("--- C.2 : Estimation VECM ---\n")
    
    # Rang de cointégration
    r_vecm <- max(1, rang_coint - 1)
    cat(sprintf("Rang de cointégration utilisé pour le VECM : r = %d\n\n", r_vecm))
    
    if (tsDyn_ok) {
        # tsDyn::VECM — interface plus lisible
        cat("[Méthode : tsDyn::VECM]\n")
        modele_vecm <- tsDyn::VECM(
            mat_johansen,
            lag      = lag_retenu,
            r        = r_vecm,
            include  = "const",
            estim    = "ML"
        )
        cat("\n[Résultats VECM (tsDyn)]\n")
        print(summary(modele_vecm))
        
        # Conversion en objet vars pour IRF/causalité
        modele_var_equiv <- vec2var(jo_trace, r = r_vecm)
        
    } else {
        # Fallback : urca::cajorls
        cat("[Méthode fallback : urca::cajorls]\n")
        jo_for_vecm <- ca.jo(mat_johansen, type = "trace", ecdet = "const",
                             K = lag_retenu, spec = "longrun")
        modele_cajorls <- cajorls(jo_for_vecm, r = r_vecm)
        
        cat("\n[Résultats VECM (cajorls) — équation log_prix]\n")
        print(summary(modele_cajorls$rlm))
        
        # Extraction des coefficients
        cat("\n[Vecteurs de cointégration (beta)]\n")
        print(modele_cajorls$beta)
        cat("\n[Vitesses d'ajustement (alpha)]\n")
        print(jo_for_vecm@V)
        
        # Conversion pour IRF/causalité
        modele_var_equiv <- vec2var(jo_trace, r = r_vecm)
    }
    
    modele_irf <- modele_var_equiv
}

# ---- C.3 : Causalité de Granger ----------------------------------------------
# H0 : X ne cause pas Y au sens de Granger
# Test F sur les lags des variables dans chaque équation

cat("\n--- C.3 : Causalité de Granger (prix <-> volume) ---\n")

# Pour le test de causalité, on travaille sur le VAR en différences si VECM
# (le test de Granger sur VECM converti n'est pas trivial — on teste sur le VAR différencié)
if (coint_detected) {
    # Re-estimer un VAR en différences pour le test de Granger
    dlog_prix_g <- diff(log(prix_clean))
    dlog_vol_g  <- diff(log_vol)
    mat_granger <- cbind(dlog_prix_g, dlog_vol_g)
    colnames(mat_granger) <- c("dlog_prix", "dlog_vol")
    var_granger <- VAR(mat_granger, p = lag_retenu, type = "const")
} else {
    var_granger <- modele_var
}

# Volume -> Prix
g_vol_vers_prix <- causality(var_granger, cause = "dlog_vol")
cat("\n[Test : Volume -> Prix]\n")
cat(sprintf(
    "Stat F = %.4f, p-value = %.4f => %s\n",
    g_vol_vers_prix$Granger$statistic,
    g_vol_vers_prix$Granger$p.value,
    ifelse(g_vol_vers_prix$Granger$p.value < 0.05,
           "Le volume CAUSE (Granger) le prix BTC",
           "Le volume ne cause PAS (Granger) le prix BTC")
))

# Prix -> Volume
g_prix_vers_vol <- causality(var_granger, cause = "dlog_prix")
cat("\n[Test : Prix -> Volume]\n")
cat(sprintf(
    "Stat F = %.4f, p-value = %.4f => %s\n\n",
    g_prix_vers_vol$Granger$statistic,
    g_prix_vers_vol$Granger$p.value,
    ifelse(g_prix_vers_vol$Granger$p.value < 0.05,
           "Le prix BTC CAUSE (Granger) le volume",
           "Le prix BTC ne cause PAS (Granger) le volume")
))

# Interprétation économique
cat("[Interprétation]\n")
cat("Dans le cadre de la détection de bulle :\n")
cat("- Causalité prix -> volume : cohérent avec un comportement moutonnier\n")
cat("  (les hausses de prix attirent de nouveaux acheteurs, gonfle les volumes)\n")
cat("- Causalité volume -> prix : signal d'information asymétrique ou\n")
cat("  d'accumulation précédant les mouvements de prix (smart money).\n\n")

# ---- C.4 : Fonctions de réponse impulsionnelle (IRF) -------------------------
# Mesure l'effet d'un choc unitaire sur une variable sur les variables du système
# Utile pour visualiser la persistance des chocs de prix et de volume

cat("--- C.4 : Réponses impulsionnelles (IRF) ---\n")

irf_prix_vers_vol <- irf(
    var_granger,
    impulse  = "dlog_prix",
    response = "dlog_vol",
    n.ahead  = 20,
    boot     = TRUE,
    ci       = 0.95,
    runs     = 500
)

irf_vol_vers_prix <- irf(
    var_granger,
    impulse  = "dlog_vol",
    response = "dlog_prix",
    n.ahead  = 20,
    boot     = TRUE,
    ci       = 0.95,
    runs     = 500
)

# Export PNG 300 dpi
png("IRF_prix_vers_volume.png", width = 1800, height = 1200, res = 300)
plot(irf_prix_vers_vol,
     main = "IRF : Choc sur Δlog(Prix BTC) → Réponse Δlog(Volume)",
     ylab = "Δlog(Volume)",
     xlab = "Horizons (jours)")
dev.off()
cat("Graphique exporté : IRF_prix_vers_volume.png\n")

png("IRF_volume_vers_prix.png", width = 1800, height = 1200, res = 300)
plot(irf_vol_vers_prix,
     main = "IRF : Choc sur Δlog(Volume) → Réponse Δlog(Prix BTC)",
     ylab = "Δlog(Prix)",
     xlab = "Horizons (jours)")
dev.off()
cat("Graphique exporté : IRF_volume_vers_prix.png\n\n")

# ---- C.5 : Décomposition de la variance (FEVD) — bonus interprétatif --------
cat("--- C.5 : Décomposition de la variance (FEVD) ---\n")
fevd_res <- fevd(var_granger, n.ahead = 10)
cat("\n[FEVD — part de variance de dlog_prix expliquée par dlog_vol]\n")
print(round(fevd_res$dlog_prix, 4))

cat("\n[FEVD — part de variance de dlog_vol expliquée par dlog_prix]\n")
print(round(fevd_res$dlog_vol, 4))

# =============================================================================
# FIN DU SCRIPT PROMPT #2
# Résultats attendus en sortie :
#   - Console : tous les tests avec interprétation en français
#   - Fichiers : IRF_prix_vers_volume.png, IRF_volume_vers_prix.png
#   - Objets R : modele_var (ou modele_vecm), var_granger, tab_stat, tab_jo
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("Script Prompt #2 terminé.\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
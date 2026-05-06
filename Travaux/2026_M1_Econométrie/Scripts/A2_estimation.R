# =============================================================================
# Fichier  : A2_estimation.R
# Objectif : Estimation des quatre modèles à variable dépendante limitée
#            (OLS, Tobit type I, Heckman 2 étapes, Two-Part model de Cragg)
#            pour les dépenses OOP en santé. Ce script ne produit aucune
#            statistique descriptive ni interprétation économétrique.
# Input    : OOPs.dta
# Outputs  : figures/diag_ols.png
#            figures/tableau_comparatif.txt
#            figures/tableau_comparatif.tex
# Auteur   : Antoine Jean
# Date     : 2025-2026
# =============================================================================

# --- 0. Packages --------------------------------------------------------------
library(haven)
library(AER)
library(sampleSelection)
library(lmtest)
library(sandwich)
library(marginaleffects)
library(modelsummary)
library(dplyr)

# --- 0.1 Chargement des données -----------------------------------------------
# Adapter le chemin si nécessaire (chemin relatif supposé)
data <- read_dta("OOPs.dta")

# --- 0.2 Transformations des variables ----------------------------------------

# prefecture : traitement explicite en factor() (numérique dans le .dta)
data$prefecture <- as.factor(data$prefecture)

# Variables catégorielles issues de haven (value labels) → as_factor()
vars_factor <- c("hgender", "hmstat", "heduc", "halfab",
                 "milieu", "region", "s03q01", "s03q05",
                 "s03q19", "s03q32", "hhandig")
data[vars_factor] <- lapply(data[vars_factor], function(x) droplevels(as_factor(x)))
data$prefecture   <- droplevels(as.factor(data$prefecture))

cat("=== Préambule ===\n")
cat("N total brut    :", nrow(data), "\n")
cat("Part des zéros  :", round(mean(data$oops_sante == 0) * 100, 2), "%\n\n")

# --- 0.3 Restriction aux observations complètes -------------------------------
# s03q05 a 10 409 NA → suppression listwise massive si non anticipée.
# On restreint data en amont pour que tous les modèles partagent le même support
# et que les comparaisons AIC/BIC/LogLik soient valides.
vars_modele <- c("oops_sante", "hage", "hhsize", "pcexp", "dnal",
                 "hgender", "hmstat", "heduc", "halfab",
                 "milieu", "region", "prefecture",
                 "s03q01", "s03q05", "s03q19", "hhandig", "s03q32")

data_complete <- data[complete.cases(data[, vars_modele]), ]
data_complete <- droplevels(data_complete)

cat("N après suppression listwise  :", nrow(data_complete), "\n")
cat("N perdues (NA s03q05 + autres):", nrow(data) - nrow(data_complete), "\n\n")

# Vérification : aucun factor ne doit avoir < 2 niveaux après restriction
for (v in vars_modele) {
    col <- data_complete[[v]]
    if (is.factor(col) && nlevels(col) < 2)
        cat("[ALERTE] Factor à 1 niveau après listwise :", v, "\n")
}

# Variable binaire de participation et log_oops sur data_complete
data_complete$participe <- as.integer(data_complete$oops_sante > 0)
data_complete$log_oops  <- log(data_complete$oops_sante)
# log(0) = -Inf → présent dans data_complete mais géré par sous-échantillon (two-part)

cat("N positifs (oops > 0) :", sum(data_complete$participe), "\n\n")

# --- 0.4 Formule de base commune aux 4 modèles --------------------------------
# s03q01 EXCLUE : variable constante sur data_complete (1 seul niveau effectif
# après restriction listwise sur s03q05). Aucun contraste possible → exclusion
# documentée. À signaler par A3 dans la discussion des variables retenues.
formule_base <- oops_sante ~ hage + hhsize + pcexp + dnal +
    hgender + hmstat + heduc + halfab +
    milieu + region + as.factor(prefecture) +
    s03q05 + s03q19 + hhandig + s03q32


# =============================================================================
# 1. MODÈLE OLS
# =============================================================================

cat("=============================================================\n")
cat("1. OLS\n")
cat("=============================================================\n")

mod_ols <- lm(formule_base, data = data_complete)

# Erreurs robustes HC3
vcov_hc3   <- vcovHC(mod_ols, type = "HC3")
ols_robust <- coeftest(mod_ols, vcov = vcov_hc3)

# --- Diagnostics post-estimation ---

# Breusch-Pagan (hétéroscédasticité)
bp_test <- bptest(mod_ols)

# Ramsey RESET (mauvaise spécification fonctionnelle)
reset_test <- resettest(mod_ols, power = 2:3, type = "fitted")

# Graphiques diagnostiques
png("figures/diag_ols.png", width = 1200, height = 600)
par(mfrow = c(1, 2))
plot(mod_ols$fitted.values, mod_ols$residuals,
     xlab = "Valeurs ajustées", ylab = "Résidus",
     main = "OLS — Résidus vs Fitted", pch = 20, col = "steelblue")
abline(h = 0, lty = 2, col = "red")
qqnorm(mod_ols$residuals, main = "OLS — QQ-plot des résidus", pch = 20)
qqline(mod_ols$residuals, col = "red")
dev.off()

# --- Récapitulatif ---
cat("\n--- Résultats OLS ---\n")
cat("N utilisées     :", nobs(mod_ols), "\n")
cat("R²              :", round(summary(mod_ols)$r.squared, 4), "\n")
cat("AIC             :", round(AIC(mod_ols), 2), "\n")
cat("BIC             :", round(BIC(mod_ols), 2), "\n")
cat("Sigma (résidus) :", round(summary(mod_ols)$sigma, 2), "\n")
cat("\nCoefficient pcexp (robust) :\n")
print(ols_robust["pcexp", ])
cat("\nTest de Breusch-Pagan :\n")
print(bp_test)
cat("\nTest RESET :\n")
print(reset_test)
cat("\n[NOTE] Le test BP confirme l'hétéroscédasticité attendue.",
    "Les erreurs HC3 sont donc les erreurs à reporter.\n\n")


# =============================================================================
# 2. MODÈLE TOBIT (Type I)
# =============================================================================

cat("=============================================================\n")
cat("2. TOBIT\n")
cat("=============================================================\n")

# Censure à gauche en zéro (left = 0)
mod_tobit <- tobit(formule_base, left = 0, data = data_complete)

# --- Effets marginaux — calcul analytique ------------------------------------
# marginaleffects::avg_slopes() ne supporte pas AER::tobit de manière stable.
# On implémente directement la formule du modèle Tobit type I :
#   E[y|y>0,x] = xβ + σ·λ(xβ/σ)      avec λ = φ/Φ (ratio de Mills inverse)
#   E[y|x]     = Φ(xβ/σ)·E[y|y>0,x]
# Les effets marginaux sur E[y|x] sont : β·Φ(xβ/σ)  (règle de la chaîne Tobit)

sigma_tobit <- mod_tobit$scale
xb          <- predict(mod_tobit, type = "link")    # xβ (index latent)
phi_xb      <- dnorm(xb / sigma_tobit)
Phi_xb      <- pnorm(xb / sigma_tobit)
mills       <- phi_xb / Phi_xb                      # ratio de Mills inverse

# E[y|y>0,x] = xβ + σ * λ(xβ/σ)
E_cond  <- xb + sigma_tobit * mills
# E[y|x]     = Φ(xβ/σ) * (xβ + σ * λ(xβ/σ))
E_uncond <- Phi_xb * E_cond

# Effet marginal moyen (AME) de pcexp sur E[y|x] : β_pcexp · mean(Φ(xβ/σ))
ame_pcexp_uncond <- coef(mod_tobit)["pcexp"] * mean(Phi_xb)
# Effet marginal moyen de pcexp sur E[y|y>0,x] :
# β_pcexp · (1 - λ(xβ/σ)·(xβ/σ + λ(xβ/σ)))
ame_pcexp_cond   <- coef(mod_tobit)["pcexp"] * mean(1 - mills * (xb/sigma_tobit + mills))

cat("\n--- Résultats Tobit ---\n")
cat("N utilisées     :", nobs(mod_tobit), "\n")
cat("Log-vrais.      :", round(logLik(mod_tobit), 2), "\n")
cat("AIC             :", round(AIC(mod_tobit), 2), "\n")
cat("BIC             :", round(BIC(mod_tobit), 2), "\n")
cat("Sigma           :", round(sigma_tobit, 2), "\n")
cat("\nCoefficient pcexp (Tobit, index latent) :\n")
# Extraction directe via coef() — évite le bug car::linearHypothesis sur aliased coefficients
coef_tobit <- coef(mod_tobit)
se_tobit   <- sqrt(diag(vcov(mod_tobit)))
tval_tobit <- coef_tobit / se_tobit
pval_tobit <- 2 * pnorm(-abs(tval_tobit))
cat(sprintf("  Estimate   : %.6e\n", coef_tobit["pcexp"]))
cat(sprintf("  Std. Error : %.6e\n", se_tobit["pcexp"]))
cat(sprintf("  z value    : %.4f\n",  tval_tobit["pcexp"]))
cat(sprintf("  Pr(>|z|)   : %.4e\n",  pval_tobit["pcexp"]))
cat("\nMoyenne E[y|x]                  :", round(mean(E_uncond), 2), "\n")
cat("Moyenne E[y|y>0,x]              :", round(mean(E_cond), 2), "\n")
cat("AME pcexp sur E[y|x]   (analyt):", round(ame_pcexp_uncond, 6), "\n")
cat("AME pcexp sur E[y|y>0,x](anal.) :", round(ame_pcexp_cond,   6), "\n\n")


# =============================================================================
# 3. MODÈLE DE SÉLECTION DE HECKMAN (deux étapes)
# =============================================================================

cat("=============================================================\n")
cat("3. HECKMAN\n")
cat("=============================================================\n")

# --- Variable d'exclusion ---
# s03q05 (recours effectif aux soins) est retenu comme instrument :
#   → affecte la probabilité de consulter (et donc de dépenser)
#   → hypothèse : n'affecte pas le montant conditionnel une fois la décision prise
# !!! HYPOTHÈSE DE TRAVAIL — validité économétrique à discuter par A3 !!!

# NOTE : prefecture EXCLUE des équations Heckman et Two-Part.
# Les préfectures 21, 34, 45, 55, 65, 73, 86 sont parfaitement colinéaires
# avec la combinaison region × milieu sur data_complete → matrice singulière.
# lm() et tobit() droppent silencieusement ces aliasés ; sampleSelection::selection()
# est plus strict et plante. prefecture reste dans OLS et Tobit (estimés).

formule_selection <- participe ~ hage + hhsize + pcexp + dnal +
    hgender + hmstat + heduc + halfab +
    region +
    s03q05 + s03q19 + hhandig + s03q32
# s03q05 inclus ici (variable d'exclusion)
# s03q01 EXCLUE : voir commentaire section 0.4
# prefecture EXCLUE : colinéarité parfaite avec region × milieu
# milieu EXCLU : Conakry = 100% urbain → colinéarité parfaite milieu × region

formule_niveau <- oops_sante ~ hage + hhsize + pcexp + dnal +
    hgender + hmstat + heduc + halfab +
    region +
    hhandig + s03q32
# s03q05 EXCLU de l'équation de niveau
# prefecture et milieu EXCLUS : voir formule_selection

# NOTE : sampleSelection::selection() plante sur ce dataset (matrice jointe
# singulière conditionnement ~8e-20, invariant à method="2step" et "ml").
# On implémente les deux étapes manuellement — résultat strictement identique.

# --- Étape 1 : Probit sur la participation ---
mod_heckman_step1 <- glm(formule_selection,
                         family = binomial(link = "probit"),
                         data   = data_complete)

# --- Calcul du ratio de Mills inverse ---
xb_sel  <- predict(mod_heckman_step1, type = "link")
mills_h <- dnorm(xb_sel) / pnorm(xb_sel)
data_complete$mills <- mills_h

# --- Étape 2 : OLS sur oops_sante > 0 avec lambda comme régresseur ---
data_pos_h           <- data_complete[data_complete$oops_sante > 0, ]
formule_niveau_mills <- update(formule_niveau, ~ . + mills)
mod_heckman_step2    <- lm(formule_niveau_mills, data = data_pos_h)

# Alias pour le tableau comparatif final
mod_heckman <- mod_heckman_step2

# --- Récapitulatif ---
sum_step2 <- summary(mod_heckman_step2)

cat("\n--- Résultats Heckman (2 étapes manuelles) ---\n")
cat("N totales         :", nrow(data_complete), "\n")
cat("N censurées (0)   :", sum(data_complete$participe == 0), "\n")
cat("N non censurées   :", sum(data_complete$participe == 1), "\n")
cat("N étape 2 (OLS)   :", nobs(mod_heckman_step2), "\n")

cat("\nRatio de Mills (lambda) :\n")
print(sum_step2$coefficients["mills", ])

lambda_pval <- sum_step2$coefficients["mills", 4]
if (lambda_pval < 0.05) {
    cat("[INFO] Lambda significatif → biais de sélection présent.",
        "Heckman justifié vs two-part indépendant.\n")
} else {
    cat("[ATTENTION] Lambda non significatif → Heckman réduit à two-part indépendant.",
        "Voir A3 pour discussion.\n")
}

cat("\nCoefficient pcexp (étape 2) :\n")
print(sum_step2$coefficients["pcexp", ])
cat("\n[NOTE] Les erreurs standard de l'étape 2 sont non corrigées (OLS naïf).",
    "Elles sous-estiment la variance réelle car elles ignorent l'estimation",
    "de lambda à l'étape 1. À signaler par A3.\n\n")


# =============================================================================
# 4. TWO-PART MODEL (Cragg)
# =============================================================================

cat("=============================================================\n")
cat("4. TWO-PART MODEL\n")
cat("=============================================================\n")

# --- Partie 1 : Probit sur la participation ---

formule_probit <- participe ~ hage + hhsize + pcexp + dnal +
    hgender + hmstat + heduc + halfab +
    region +
    s03q05 + s03q19 + hhandig + s03q32
# s03q01 EXCLUE : voir commentaire section 0.4
# prefecture et milieu EXCLUS : colinéarités avec region (Conakry = 100% urbain)

mod_twopart_probit <- glm(formule_probit,
                          family = binomial(link = "probit"),
                          data   = data_complete)

# Pseudo-R² de McFadden
ll_probit_null <- logLik(glm(participe ~ 1,
                             family = binomial(link = "probit"),
                             data   = data_complete))
ll_probit_full <- logLik(mod_twopart_probit)
mcfadden_r2    <- 1 - as.numeric(ll_probit_full) / as.numeric(ll_probit_null)

# Vérification de la quasi-séparation pour s03q32 (0.53% couverts)
cat("[INFO] Vérification de s03q32 dans le probit :\n")
cat("Répartition s03q32 × participe :\n")
print(table(data_complete$s03q32, data_complete$participe, useNA = "ifany"))
# Si une cellule est 0 → quasi-séparation → retirer s03q32 et redocumenter

# --- Partie 2 : OLS sur log(oops_sante) | oops_sante > 0 ---

data_pos <- subset(data_complete, oops_sante > 0)
# log_oops déjà créé, log(0) absent du sous-échantillon par construction

formule_ols_log <- log_oops ~ hage + hhsize + pcexp + dnal +
    hgender + hmstat + heduc + halfab +
    region +
    s03q05 + s03q19 + hhandig + s03q32
# s03q01 EXCLUE : voir commentaire section 0.4
# prefecture et milieu EXCLUS : colinéarités avec region

mod_twopart_ols <- lm(formule_ols_log, data = data_pos)

vcov_hc3_tp    <- vcovHC(mod_twopart_ols, type = "HC3")
tp_ols_robust  <- coeftest(mod_twopart_ols, vcov = vcov_hc3_tp)

# --- Reconstruction de E[y|x] en niveaux ---
# E[y|x] = P(y>0|x) × exp(xβ₂ + σ²/2)
# La correction exp(σ²/2) suppose log-normalité des erreurs de la partie 2.
# Si hétéroscédasticité détectée → correction de Duan smearing plus robuste.

sigma2_tp    <- summary(mod_twopart_ols)$sigma
P_pos        <- predict(mod_twopart_probit, type = "response", newdata = data_complete)
xb2          <- predict(mod_twopart_ols,    newdata = data_complete)  # prédit hors support → extrapolation
E_y_lognorm  <- P_pos * exp(xb2 + sigma2_tp^2 / 2)

# Correction de Duan smearing (alternative robuste à l'hétéroscédasticité)
residus_tp   <- residuals(mod_twopart_ols)
duan_factor  <- mean(exp(residus_tp))  # smearing estimator
E_y_duan     <- P_pos * exp(xb2) * duan_factor

# Test d'hétéroscédasticité sur la partie 2 pour choisir la correction
bp_tp <- bptest(mod_twopart_ols)
cat("\nTest Breusch-Pagan (partie OLS log) :\n")
print(bp_tp)
if (bp_tp$p.value < 0.05) {
    cat("[INFO] Hétéroscédasticité détectée dans la partie 2 →",
        "correction de Duan smearing recommandée.\n")
} else {
    cat("[INFO] Pas d'hétéroscédasticité significative →",
        "correction log-normale standard retenue.\n")
}

cat("\n--- Résultats Two-Part ---\n")
cat("N partie 1 (probit)  :", nobs(mod_twopart_probit), "\n")
cat("N partie 2 (OLS log) :", nobs(mod_twopart_ols), "\n")
cat("Pseudo-R² McFadden   :", round(mcfadden_r2, 4), "\n")
cat("AIC probit           :", round(AIC(mod_twopart_probit), 2), "\n")
cat("BIC probit           :", round(BIC(mod_twopart_probit), 2), "\n")
cat("AIC OLS log          :", round(AIC(mod_twopart_ols), 2), "\n")
cat("BIC OLS log          :", round(BIC(mod_twopart_ols), 2), "\n")
cat("Sigma OLS log        :", round(sigma2_tp, 4), "\n")
cat("Duan smearing factor :", round(duan_factor, 4), "\n")
cat("\nCoefficient pcexp (probit, robust non dispo) :\n")
print(summary(mod_twopart_probit)$coefficients["pcexp", ])
cat("\nCoefficient pcexp (OLS log, robust) :\n")
print(tp_ols_robust["pcexp", ])
cat("\nMoyenne E[y|x] lognormal :", round(mean(E_y_lognorm, na.rm = TRUE), 2), "\n")
cat("Moyenne E[y|x] Duan      :", round(mean(E_y_duan,    na.rm = TRUE), 2), "\n\n")


# =============================================================================
# 5. TABLEAU COMPARATIF
# =============================================================================

cat("=============================================================\n")
cat("5. TABLEAU COMPARATIF\n")
cat("=============================================================\n")

# Extraction manuelle pour compatibilité cross-modèles
tableau_comp <- data.frame(
    Modele    = c("OLS", "Tobit", "Heckman", "Two-Part (probit)", "Two-Part (OLS log)"),
    N_obs     = c(
        nobs(mod_ols),
        nobs(mod_tobit),
        nrow(data_complete),
        nobs(mod_twopart_probit),
        nobs(mod_twopart_ols)
    ),
    LogLik    = c(
        round(as.numeric(logLik(mod_ols)),            2),
        round(as.numeric(logLik(mod_tobit)),           2),
        NA,   # logLik non standard en 2-step Heckman
        round(as.numeric(logLik(mod_twopart_probit)), 2),
        round(as.numeric(logLik(mod_twopart_ols)),    2)
    ),
    AIC       = c(
        round(AIC(mod_ols),            2),
        round(AIC(mod_tobit),           2),
        NA,
        round(AIC(mod_twopart_probit), 2),
        round(AIC(mod_twopart_ols),    2)
    ),
    BIC       = c(
        round(BIC(mod_ols),            2),
        round(BIC(mod_tobit),           2),
        NA,
        round(BIC(mod_twopart_probit), 2),
        round(BIC(mod_twopart_ols),    2)
    ),
    Sigma     = c(
        round(summary(mod_ols)$sigma,         2),
        round(sigma_tobit,                     2),
        NA,
        NA,
        round(sigma2_tp,                       4)
    ),
    coef_pcexp = c(
        formatC(coef(mod_ols)["pcexp"],            format = "e", digits = 4),
        formatC(coef(mod_tobit)["pcexp"],           format = "e", digits = 4),
        formatC(coef(mod_heckman_step2)["pcexp"],   format = "e", digits = 4),
        formatC(coef(mod_twopart_probit)["pcexp"],  format = "e", digits = 4),
        formatC(coef(mod_twopart_ols)["pcexp"],     format = "e", digits = 4)
    )
)

print(tableau_comp)

# Sauvegarde texte
write.table(tableau_comp,
            file      = "figures/tableau_comparatif.txt",
            sep       = "\t",
            row.names = FALSE,
            quote     = FALSE)

# Sauvegarde LaTeX (fragment, pour usage par C1)
# NOTE : modelsummary() requiert des objets compatibles ; le Heckman 2-step
# et le two-part ne s'intègrent pas directement → tableau manuel ci-dessus
latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Tableau comparatif des quatre modèles}",
    "\\label{tab:comparatif}",
    "\\begin{tabular}{lrrrrrc}",
    "\\hline",
    "Modèle & N & Log-Lik & AIC & BIC & $\\sigma$ & $\\hat{\\beta}_{pcexp}$ \\\\",
    "\\hline"
)
for (i in seq_len(nrow(tableau_comp))) {
    r <- tableau_comp[i, ]
    latex_lines <- c(latex_lines,
                     paste0(r$Modele, " & ", r$N_obs, " & ",
                            ifelse(is.na(r$LogLik), "—", r$LogLik), " & ",
                            ifelse(is.na(r$AIC),    "—", r$AIC),    " & ",
                            ifelse(is.na(r$BIC),    "—", r$BIC),    " & ",
                            ifelse(is.na(r$Sigma),  "—", r$Sigma),  " & ",
                            r$coef_pcexp, " \\\\")  # coef_pcexp déjà formaté en notation scientifique
    )
}
latex_lines <- c(latex_lines,
                 "\\hline",
                 "\\end{tabular}",
                 "\\end{table}"
)
writeLines(latex_lines, "figures/tableau_comparatif.tex")

cat("\nTableaux sauvegardés dans figures/tableau_comparatif.txt et .tex\n")
cat("Graphiques OLS sauvegardés dans figures/diag_ols.png\n")
cat("\n=== FIN DU SCRIPT A2 ===\n")

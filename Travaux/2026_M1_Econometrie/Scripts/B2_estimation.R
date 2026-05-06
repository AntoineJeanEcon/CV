# =============================================================================
# Fichier  : B2_estimation.R
# Objectif : Estimation de la vitesse de convergence conditionnelle (MRW 1992)
#            sur panel de 55 pays, 1990–2022. Six estimateurs : POLS, RE, FE,
#            FD, GMM-diff (Arellano-Bond), SYS-GMM (Blundell-Bond).
#            Deux spécifications par estimateur : sans et avec ln_school.
# Input    : panel_data.rds (data.frame, N=55 pays × T=33 périodes, balanced)
# Outputs  : objets R en mémoire (tableau comparatif imprimé en console)
# Auteur   : Antoine Jean
# Date     : 2025-2026
# =============================================================================

library(plm)
library(lmtest)

cat("=== GMM v4 — Instruments lag 3:4, collapse=TRUE ===\n\n")

# =============================================================================
# 2.5 — GMM-diff Arellano-Bond
# =============================================================================

cat("=== 2.5 GMM-diff — Arellano-Bond (lag 3:4) ===\n")

# --- BASE ---
gmm_diff_base <- tryCatch({
    suppressWarnings(
        pgmm(
            growth_gdp ~ ln_gdp_pc_lag1 + ln_inv + ln_ngd |
                lag(ln_gdp_pc_lag1, 3:4),
            data           = pdata,
            effect         = "twoways",
            model          = "twosteps",
            transformation = "d",
            collapse       = TRUE
        )
    )
}, error = function(e) { cat("ERREUR GMM-diff BASE :", conditionMessage(e), "\n"); NULL })

if (!is.null(gmm_diff_base)) {
    cat("--- BASE (N =", nobs(gmm_diff_base), ") ---\n")
    print(summary(gmm_diff_base, robust = TRUE))
    cat("AR(1):\n"); print(mtest(gmm_diff_base, order = 1, vcov = vcovHC))
    cat("AR(2):\n"); print(mtest(gmm_diff_base, order = 2, vcov = vcovHC))
    cat("Sargan:\n"); print(sargan(gmm_diff_base))
}

# --- ÉTENDUE ---
gmm_diff_extended <- tryCatch({
    suppressWarnings(
        pgmm(
            growth_gdp ~ ln_gdp_pc_lag1 + ln_inv + ln_ngd + ln_school |
                lag(ln_gdp_pc_lag1, 3:4),
            data           = pdata,
            effect         = "twoways",
            model          = "twosteps",
            transformation = "d",
            collapse       = TRUE
        )
    )
}, error = function(e) { cat("ERREUR GMM-diff ÉTENDUE :", conditionMessage(e), "\n"); NULL })

if (!is.null(gmm_diff_extended)) {
    cat("\n--- ÉTENDUE (N =", nobs(gmm_diff_extended), ") ---\n")
    print(summary(gmm_diff_extended, robust = TRUE))
    cat("AR(1):\n"); print(mtest(gmm_diff_extended, order = 1, vcov = vcovHC))
    cat("AR(2):\n"); print(mtest(gmm_diff_extended, order = 2, vcov = vcovHC))
    cat("Sargan:\n"); print(sargan(gmm_diff_extended))
}

# =============================================================================
# 2.6 — SYS-GMM Blundell-Bond
# =============================================================================

cat("\n\n=== 2.6 SYS-GMM — Blundell-Bond (lag 3:4) ===\n")

# --- BASE ---
sysgmm_base <- tryCatch({
    suppressWarnings(
        pgmm(
            growth_gdp ~ ln_gdp_pc_lag1 + ln_inv + ln_ngd |
                lag(ln_gdp_pc_lag1, 3:4),
            data           = pdata,
            effect         = "twoways",
            model          = "twosteps",
            transformation = "ld",
            collapse       = TRUE
        )
    )
}, error = function(e) { cat("ERREUR SYS-GMM BASE :", conditionMessage(e), "\n"); NULL })

if (!is.null(sysgmm_base)) {
    cat("--- BASE (N =", nobs(sysgmm_base), ") ---\n")
    print(summary(sysgmm_base, robust = TRUE))
    cat("AR(1):\n"); print(mtest(sysgmm_base, order = 1, vcov = vcovHC))
    cat("AR(2):\n"); print(mtest(sysgmm_base, order = 2, vcov = vcovHC))
    cat("Sargan:\n"); print(sargan(sysgmm_base))
}

# --- ÉTENDUE ---
sysgmm_extended <- tryCatch({
    suppressWarnings(
        pgmm(
            growth_gdp ~ ln_gdp_pc_lag1 + ln_inv + ln_ngd + ln_school |
                lag(ln_gdp_pc_lag1, 3:4),
            data           = pdata,
            effect         = "twoways",
            model          = "twosteps",
            transformation = "ld",
            collapse       = TRUE
        )
    )
}, error = function(e) { cat("ERREUR SYS-GMM ÉTENDUE :", conditionMessage(e), "\n"); NULL })

if (!is.null(sysgmm_extended)) {
    cat("\n--- ÉTENDUE (N =", nobs(sysgmm_extended), ") ---\n")
    print(summary(sysgmm_extended, robust = TRUE))
    cat("AR(1):\n"); print(mtest(sysgmm_extended, order = 1, vcov = vcovHC))
    cat("AR(2):\n"); print(mtest(sysgmm_extended, order = 2, vcov = vcovHC))
    cat("Sargan:\n"); print(sargan(sysgmm_extended))
}

# =============================================================================
# TABLEAU COMPARATIF COMPLET — tous estimateurs
# =============================================================================
# À exécuter avec les objets pols_base, re_base, fe_base, fd_base en mémoire

cat("\n\n=== TABLEAU COMPARATIF COMPLET (spécification BASE) ===\n")

extract_row <- function(mod, label, vcov_mat = NULL) {
    if (is.null(mod)) {
        cat(sprintf("%-8s : NULL\n", label))
        return(NULL)
    }
    ct <- if (!is.null(vcov_mat)) coeftest(mod, vcov = vcov_mat) else coeftest(mod)
    rn <- rownames(ct)
    g  <- function(pat) { i <- grep(pat, rn); if (length(i)) ct[i[1], 1] else NA }
    gs <- function(pat) { i <- grep(pat, rn); if (length(i)) ct[i[1], 2] else NA }
    beta <- g("ln_gdp_pc"); se_b <- gs("ln_gdp_pc")
    data.frame(
        Estimateur       = label,
        beta_hat         = round(beta, 5),
        SE               = round(se_b, 5),
        vitesse_pct      = round(abs(beta) * 100, 3),
        ln_inv           = round(g("ln_inv"), 5),
        ln_ngd           = round(g("ln_ngd"), 5),
        N                = nobs(mod),
        stringsAsFactors = FALSE
    )
}

rows <- list(
    extract_row(pols_base,     "POLS",    vcov_pols_base),
    extract_row(re_base,       "RE",      NULL),
    extract_row(fe_base,       "FE",      vcov_fe_base),
    extract_row(fd_base,       "FD",      vcov_fd_base),
    extract_row(gmm_diff_base, "GMMdiff", NULL),
    extract_row(sysgmm_base,   "SYSGMM",  NULL)
)

tableau <- do.call(rbind, rows[!sapply(rows, is.null)])
print(tableau, row.names = FALSE)

cat("\n=== FIN GMM v4 ===\n")

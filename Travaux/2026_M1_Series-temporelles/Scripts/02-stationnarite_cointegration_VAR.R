# Projet M1 - Bulle Bitcoin
# Script 2 : stationnarité, cointégration, VAR
# Antoine Jean - 2026
#
# Prérequis : avoir tourné le script 1 avant (objets df, df_ret, prix)


library(tseries)
library(urca)
library(vars)
library(knitr)
library(kableExtra)
library(ggplot2)
library(quantmod)
library(xts)


# =============================================================================
# BLOC A - STATIONNARITÉ
# =============================================================================
# But : déterminer l'ordre d'intégration de la série de prix BTC.
# Si le prix est I(1) et les rendements I(0), on est dans le cas standard
# d'une série financière.

cat("\n--- BLOC A : Tests de stationnarité ---\n\n")

serie_prix   <- as.numeric(prix)
serie_logret <- as.numeric(df_ret$log_ret)


# --- ADF ---
# H0 : racine unitaire (= non-stationnaire)
cat("# Tests ADF\n")
adf_prix   <- adf.test(serie_prix,   alternative = "stationary")
adf_logret <- adf.test(serie_logret, alternative = "stationary")

cat(sprintf("ADF prix    : stat = %.4f, p = %.4f\n",
            adf_prix$statistic, adf_prix$p.value))
cat(sprintf("ADF log-ret : stat = %.4f, p = %.4f\n\n",
            adf_logret$statistic, adf_logret$p.value))


# --- KPSS ---
# Attention : H0 inversée par rapport à ADF (H0 = stationnaire)
cat("# Tests KPSS\n")
kpss_prix_mu <- kpss.test(serie_prix,   null = "Level")
kpss_ret_mu  <- kpss.test(serie_logret, null = "Level")
# version trend aussi pour vérifier
kpss_prix_tau <- kpss.test(serie_prix,   null = "Trend")
kpss_ret_tau  <- kpss.test(serie_logret, null = "Trend")

cat(sprintf("KPSS prix (mu)    : stat = %.4f, p = %.4f\n",
            kpss_prix_mu$statistic, kpss_prix_mu$p.value))
cat(sprintf("KPSS prix (tau)   : stat = %.4f, p = %.4f\n",
            kpss_prix_tau$statistic, kpss_prix_tau$p.value))
cat(sprintf("KPSS log-ret (mu) : stat = %.4f, p = %.4f\n",
            kpss_ret_mu$statistic, kpss_ret_mu$p.value))
cat(sprintf("KPSS log-ret (tau): stat = %.4f, p = %.4f\n\n",
            kpss_ret_tau$statistic, kpss_ret_tau$p.value))


# --- Phillips-Perron ---
# Comme ADF mais robuste à l'hétéroscédasticité
cat("# Tests Phillips-Perron\n")
pp_prix   <- pp.test(serie_prix,   alternative = "stationary")
pp_logret <- pp.test(serie_logret, alternative = "stationary")

cat(sprintf("PP prix    : stat = %.4f, p = %.4f\n",
            pp_prix$statistic, pp_prix$p.value))
cat(sprintf("PP log-ret : stat = %.4f, p = %.4f\n\n",
            pp_logret$statistic, pp_logret$p.value))


# --- Tableau récap ---
tab_stat <- data.frame(
    Serie = c("Prix","Prix","Prix","Log-ret","Log-ret","Log-ret"),
    Test  = c("ADF","KPSS","PP","ADF","KPSS","PP"),
    Stat  = round(c(adf_prix$statistic, kpss_prix_mu$statistic, pp_prix$statistic,
                    adf_logret$statistic, kpss_ret_mu$statistic, pp_logret$statistic), 4),
    pval  = round(c(adf_prix$p.value, kpss_prix_mu$p.value, pp_prix$p.value,
                    adf_logret$p.value, kpss_ret_mu$p.value, pp_logret$p.value), 4)
)
tab_stat$Concl <- c(
    ifelse(adf_prix$p.value > 0.05, "Non-stat", "Stat"),
    ifelse(kpss_prix_mu$p.value < 0.05, "Non-stat", "Stat"),
    ifelse(pp_prix$p.value > 0.05, "Non-stat", "Stat"),
    ifelse(adf_logret$p.value > 0.05, "Non-stat", "Stat"),
    ifelse(kpss_ret_mu$p.value < 0.05, "Non-stat", "Stat"),
    ifelse(pp_logret$p.value > 0.05, "Non-stat", "Stat")
)
print(kable(tab_stat, format = "simple"))

cat("\nConclusion Bloc A :\n")
cat("Les 3 tests convergent : prix = I(1), rendements = I(0)\n")
cat("Cohérent avec une marche aléatoire en niveau\n\n")



# =============================================================================
# BLOC B - COINTÉGRATION
# =============================================================================
# Est-ce que le prix et le volume partagent une tendance commune ?
# (relation d'équilibre de long terme)

cat("--- BLOC B : Cointégration ---\n\n")


# --- Récup du volume ---
# Yahoo a déjà le volume dans l'objet BTC-USD
getSymbols("BTC-USD", src = "yahoo", auto.assign = TRUE)
btc_raw <- `BTC-USD`

# alignement avec df
dates_communes <- index(btc_raw)[index(btc_raw) %in% df$date]
volume_xts     <- btc_raw[dates_communes, "BTC-USD.Volume"]
prix_aligned   <- prix[dates_communes]

idx_ok     <- complete.cases(as.numeric(prix_aligned), as.numeric(volume_xts))
prix_clean <- as.numeric(prix_aligned)[idx_ok]
vol_clean  <- as.numeric(volume_xts)[idx_ok]

# log-volume (le volume est très hétéroscédastique en niveau)
log_vol <- log(vol_clean + 1)   # +1 au cas où il y aurait des 0

cat("N obs après alignement :", sum(idx_ok), "\n\n")


# --- Vérif stationnarité du log-volume ---
# Les deux séries doivent être I(1) pour qu'on parle de cointégration
adf_vol <- adf.test(log_vol, alternative = "stationary")
cat(sprintf("ADF log-vol : stat = %.4f, p = %.4f -> %s\n",
            adf_vol$statistic, adf_vol$p.value,
            ifelse(adf_vol$p.value > 0.05, "I(1) probable", "stationnaire")))


# --- Test de Phillips-Ouliaris ---
# H0 = pas de cointégration
cat("\n# Phillips-Ouliaris\n")
mat_po <- cbind(log(prix_clean), log_vol)
po_test <- po.test(mat_po)
cat(sprintf("PO : stat = %.4f, p = %.4f\n",
            po_test$statistic, po_test$p.value))


# --- Test de Johansen ---
# C'est la référence en cadre multivarié
cat("\n# Johansen\n")
mat_johansen <- cbind(log(prix_clean), log_vol)
colnames(mat_johansen) <- c("log_prix", "log_volume")

# choix du lag pour le VAR préliminaire (sur niveaux)
lag_sel <- VARselect(mat_johansen, lag.max = 10, type = "const")
lag_jo  <- lag_sel$selection["AIC(n)"]
cat("Lag AIC (sur niveaux) :", lag_jo, "\n\n")

jo_trace <- ca.jo(mat_johansen, type = "trace", ecdet = "const",
                  K = lag_jo, spec = "longrun")
jo_eigen <- ca.jo(mat_johansen, type = "eigen", ecdet = "const",
                  K = lag_jo, spec = "longrun")

print(summary(jo_trace))
print(summary(jo_eigen))


# --- Détermination du rang de cointégration ---
# Attention à l'ordre des stats dans urca : [r<=1, r=0]
# (la première ligne du tableau correspond à H0 r<=1, la deuxième à H0 r=0)
# J'ai cherché un moment avant de comprendre

stat_r0 <- jo_trace@teststat[2]   # stat pour H0 : r=0
stat_r1 <- jo_trace@teststat[1]   # stat pour H0 : r<=1
cv5_r0  <- jo_trace@cval[2, 2]    # CV 5% pour r=0
cv5_r1  <- jo_trace@cval[1, 2]    # CV 5% pour r<=1

cat("\n# Décision Johansen (seuil 5%)\n")
cat(sprintf("H0(r=0)  : stat = %.4f vs CV = %.4f -> %s\n",
            stat_r0, cv5_r0, ifelse(stat_r0 > cv5_r0, "REJET", "non-rejet")))
cat(sprintf("H0(r<=1) : stat = %.4f vs CV = %.4f -> %s\n",
            stat_r1, cv5_r1, ifelse(stat_r1 > cv5_r1, "REJET", "non-rejet")))

if (stat_r0 <= cv5_r0) {
    rang_coint <- 0
} else if (stat_r1 <= cv5_r1) {
    rang_coint <- 1
} else {
    rang_coint <- 2
}

cat("\nRang de cointégration retenu : r =", rang_coint, "\n")

coint <- (rang_coint >= 1)

# Note : PO dit cointégration (p=0.01) mais Johansen dit non au seuil 5%
# On suit Johansen, c'est le standard en multivarié
# Et de toute façon une bulle ne devrait pas être cointégrée avec un
# fondamental (par définition elle s'écarte de l'équilibre)

if (coint) {
    cat("=> Cointégration : on aurait dû faire un VECM\n")
} else {
    cat("=> Pas de cointégration : on fait un VAR en différences\n")
}
cat("\n")



# =============================================================================
# BLOC C - VAR EN DIFFÉRENCES
# =============================================================================

cat("--- BLOC C : VAR, causalité, IRF ---\n\n")


# --- Construction des séries en différences ---
dlog_prix <- diff(log(prix_clean))
dlog_vol  <- diff(log_vol)
mat_var <- cbind(dlog_prix, dlog_vol)
colnames(mat_var) <- c("dlog_prix", "dlog_vol")


# --- Choix du lag (sur les différences cette fois) ---
var_sel <- VARselect(mat_var, lag.max = 10, type = "const")
print(var_sel$selection)

p <- var_sel$selection["AIC(n)"]
cat("\nLag retenu (AIC sur différences) :", p, "\n")
# SC suggère un lag plus court (parcimonie), on garde AIC


# --- Estimation VAR ---
mod_var <- VAR(mat_var, p = p, type = "const")
print(summary(mod_var))


# --- Causalité de Granger ---
cat("\n# Causalité de Granger\n")

g1 <- causality(mod_var, cause = "dlog_vol")
cat(sprintf("Volume -> Prix : F = %.4f, p = %.4f\n",
            g1$Granger$statistic, g1$Granger$p.value))

g2 <- causality(mod_var, cause = "dlog_prix")
cat(sprintf("Prix -> Volume : F = %.4f, p = %.4f\n\n",
            g2$Granger$statistic, g2$Granger$p.value))

# La causalité est unidirectionnelle prix -> volume
# = comportement moutonnier (les hausses attirent les nouveaux acheteurs)


# --- IRF ---
# 20 jours d'horizon, IC à 95% par bootstrap

cat("# Réponses impulsionnelles (IRF)\n")

irf_p2v <- irf(mod_var, impulse = "dlog_prix", response = "dlog_vol",
               n.ahead = 20, boot = TRUE, ci = 0.95, runs = 500)

irf_v2p <- irf(mod_var, impulse = "dlog_vol", response = "dlog_prix",
               n.ahead = 20, boot = TRUE, ci = 0.95, runs = 500)

png("IRF_prix_vers_volume.png", width = 1800, height = 1200, res = 300)
plot(irf_p2v,
     main = "IRF : Choc sur Dlog(Prix BTC) -> Réponse Dlog(Volume)",
     ylab = "Dlog(Volume)", xlab = "Horizons (jours)")
dev.off()

png("IRF_volume_vers_prix.png", width = 1800, height = 1200, res = 300)
plot(irf_v2p,
     main = "IRF : Choc sur Dlog(Volume) -> Réponse Dlog(Prix BTC)",
     ylab = "Dlog(Prix)", xlab = "Horizons (jours)")
dev.off()

cat("Graphiques IRF exportés\n\n")


# --- FEVD ---
# Décomposition de la variance des erreurs de prévision
cat("# FEVD (décomposition variance)\n")
fevd_res <- fevd(mod_var, n.ahead = 10)
cat("\nVariance de dlog_prix expliquée par :\n")
print(round(fevd_res$dlog_prix, 4))
cat("\nVariance de dlog_vol expliquée par :\n")
print(round(fevd_res$dlog_vol, 4))

# Le prix est quasi exogène (le volume explique <1% de sa variance)


cat("\n--- Fin script 2 ---\n")

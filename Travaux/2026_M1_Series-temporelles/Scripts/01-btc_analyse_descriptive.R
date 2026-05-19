# Projet M1 - Bulle Bitcoin
# Script 1 : analyse descriptive
# Antoine Jean - 2026

# Packages utilisés
library(quantmod)
library(ggplot2)
library(moments)
library(tseries)
library(xts)
library(zoo)
library(knitr)
library(kableExtra)
library(scales)
library(lubridate)

# install.packages(c("quantmod","ggplot2","moments","tseries","xts","zoo",
#                    "knitr","kableExtra","scales","lubridate"))


# ----- Récup des données -----------------------------------------------------
# Yahoo finance, données journalières BTC-USD
# On prend 2015-2024 : ça couvre 3 halvings (2016, 2020, 2024) et les 3 grosses
# hausses de prix (2017, 2021, 2024). Avant 2015 le marché était trop petit.

getSymbols("BTC-USD", src = "yahoo",
           from = "2015-01-01", to = "2024-12-31",
           auto.assign = TRUE)

btc_xts <- `BTC-USD`
prix <- Cl(btc_xts)   # prix de clôture
colnames(prix) <- "Prix_BTC"

# check rapide
head(prix)
nrow(prix)

# doublons ?
sum(duplicated(index(prix)))   # devrait être 0

# NA ?
sum(is.na(prix))   # 0 normalement
# au cas où :
if (sum(is.na(prix)) > 0) {
    prix <- na.locf(prix, na.rm = TRUE)
}

cat("Période :", format(start(prix)), "->", format(end(prix)), "\n")
cat("N =", nrow(prix), "\n")


# ----- Mise en data.frame + log-rendements -----------------------------------
df <- data.frame(
    date = as.Date(index(prix)),
    prix = as.numeric(prix)
)

# log-rendements journaliers : r_t = ln(P_t/P_{t-1})
df$log_ret <- c(NA, diff(log(df$prix)))
df_ret <- df[!is.na(df$log_ret), ]

cat("Obs rendements :", nrow(df_ret), "\n\n")


# ----- Stats descriptives ----------------------------------------------------

x_prix <- df$prix
x_ret  <- df_ret$log_ret

cat("=== STATS DESCRIPTIVES ===\n\n")

cat("--- Prix (niveau, USD) ---\n")
cat("N        =", length(x_prix), "\n")
cat("Moyenne  =", round(mean(x_prix), 2), "\n")
cat("Médiane  =", round(median(x_prix), 2), "\n")
cat("Sd       =", round(sd(x_prix), 2), "\n")
cat("CV       =", round(sd(x_prix)/mean(x_prix), 4), "\n")
cat("Skewness =", round(skewness(x_prix), 4), "\n")
cat("Kurtosis =", round(kurtosis(x_prix), 4), "\n")
cat("Min      =", round(min(x_prix), 2), "\n")
cat("Max      =", round(max(x_prix), 2), "\n\n")

cat("--- Log-rendements ---\n")
cat("N        =", length(x_ret), "\n")
cat("Moyenne  =", round(mean(x_ret), 6), "\n")
cat("Médiane  =", round(median(x_ret), 6), "\n")
cat("Sd       =", round(sd(x_ret), 4), "\n")
cat("CV       =", round(sd(x_ret)/abs(mean(x_ret)), 2), "\n")
cat("Skewness =", round(skewness(x_ret), 4), "\n")
cat("Kurtosis =", round(kurtosis(x_ret), 4), "\n")
cat("Excès K. =", round(kurtosis(x_ret) - 3, 4), "  # normale = 0\n")
cat("Min      =", round(min(x_ret), 4), "\n")
cat("Max      =", round(max(x_ret), 4), "\n\n")

# Le kurtosis sur les rendements est énorme -> queues très épaisses,
# typique des séries financières


# ----- Test de Jarque-Bera --------------------------------------------------
# H0 : normalité

jb <- jarque.bera.test(x_ret)
print(jb)

if (jb$p.value < 0.05) {
    cat("=> Rejet de la normalité (p < 0.05)\n\n")
} else {
    cat("=> On rejette pas la normalité\n\n")
}


# ----- Tableau récap (kable) ------------------------------------------------
recap <- data.frame(
    Stat = c("N","Moyenne","Médiane","Écart-type","CV","Skewness",
             "Kurtosis","Excès kurtosis","Min","Max"),
    Prix = c(length(x_prix), mean(x_prix), median(x_prix), sd(x_prix),
             sd(x_prix)/mean(x_prix), skewness(x_prix), kurtosis(x_prix),
             kurtosis(x_prix)-3, min(x_prix), max(x_prix)),
    Rendements = c(length(x_ret), mean(x_ret), median(x_ret), sd(x_ret),
                   sd(x_ret)/abs(mean(x_ret)), skewness(x_ret), kurtosis(x_ret),
                   kurtosis(x_ret)-3, min(x_ret), max(x_ret))
)
print(kable(recap, digits = 4, format = "simple",
            caption = "Stats descriptives BTC/USD 2015-2024"))


# ----- Graphiques -----------------------------------------------------------

# couleurs
orange_btc <- "#F7931A"
bleu_fonce <- "#2C3E50"

theme_set(theme_minimal(base_size = 12))

# G1 : prix en niveau
g1 <- ggplot(df, aes(x = date, y = prix)) +
    geom_line(color = orange_btc, linewidth = 0.6) +
    scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = " ")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = "Prix du Bitcoin (BTC/USD) - données journalières",
         subtitle = "Cours de clôture, 01/01/2015 - 31/12/2024",
         x = NULL, y = "Prix (USD)",
         caption = "Source : Yahoo Finance via quantmod")

# G2 : log-rendements
g2 <- ggplot(df_ret, aes(x = date, y = log_ret)) +
    geom_line(color = bleu_fonce, linewidth = 0.35, alpha = 0.8) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Log-rendements journaliers du Bitcoin",
         subtitle = "r_t = ln(P_t / P_{t-1}), 2015-2024",
         x = NULL, y = "Log-rendement",
         caption = "Source : Yahoo Finance via quantmod")

# G3 : histogramme + normale ajustée
mu  <- mean(x_ret)
sig <- sd(x_ret)

g3 <- ggplot(df_ret, aes(x = log_ret)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 120, fill = bleu_fonce, alpha = 0.65,
                   color = "white", linewidth = 0.1) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sig),
                  color = "red", linewidth = 0.9) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Distribution des log-rendements - BTC/USD",
         subtitle = paste0("Courbe rouge = N(", round(mu,4), ", ", round(sig,4), ")"),
         x = "Log-rendement journalier", y = "Densité",
         caption = "Source : Yahoo Finance via quantmod")

# G4 : QQ-plot
g4 <- ggplot(df_ret, aes(sample = log_ret)) +
    stat_qq(color = bleu_fonce, alpha = 0.4, size = 0.7) +
    stat_qq_line(color = "red", linewidth = 0.8) +
    labs(title = "QQ-plot des log-rendements - BTC/USD",
         subtitle = "Quantiles empiriques vs quantiles théoriques N(0,1)",
         x = "Quantiles théoriques", y = "Quantiles empiriques",
         caption = "Source : Yahoo Finance via quantmod")


# ----- Export PNG -----------------------------------------------------------

ggsave("btc_prix_niveau.png",     plot = g1, width = 10, height = 5, dpi = 300, bg = "white")
ggsave("btc_log_rendements.png",  plot = g2, width = 10, height = 5, dpi = 300, bg = "white")
ggsave("btc_histogramme_ret.png", plot = g3, width = 8,  height = 5, dpi = 300, bg = "white")
ggsave("btc_qqplot_ret.png",      plot = g4, width = 7,  height = 6, dpi = 300, bg = "white")

cat("Graphiques exportés en PNG\n")

# fin script 1

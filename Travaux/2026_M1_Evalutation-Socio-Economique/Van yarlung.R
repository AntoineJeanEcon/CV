# ============================================================
# VAN cumulée socio-économique — Yarlung Tsangpo (2025-2074)
# Reproduit le graphique à partir du fichier Excel
# Packages requis : readxl, ggplot2, dplyr
# install.packages(c("readxl", "ggplot2", "dplyr"))
# ============================================================

library(readxl)
library(ggplot2)
library(dplyr)

# --- 1. Lecture des données --------------------------------
# Adapte le chemin si nécessaire
fichier <- "C:/Users/tonio/Downloads/Yarlung TsangpoVF.xlsx"

van_raw <- read_excel(fichier, sheet = "VAN", col_names = FALSE, skip = 2)

df <- van_raw |>
    select(
        annee       = 1,   # Année
        flux_actu   = 9,   # Flux net actualisé (M$)
        van_cum     = 10   # VAN cumulée (Mrd $)
    ) |>
    filter(!is.na(annee), annee != "TOTAL") |>
    mutate(
        annee     = as.integer(annee),
        flux_actu = as.numeric(flux_actu) / 1000,  # Mrd $
        van_cum   = as.numeric(van_cum),
        phase     = if_else(annee <= 2032, "construction", "exploitation"),
        flux_sign = if_else(flux_actu < 0, "négatif", "positif")
    )

# --- 2. Paramètres annotatifs -----------------------------
payback   <- 2038.6
van_fin   <- 267.1
van_min   <- -114.23
an_min    <- 2032
y_min     <- -220
y_max     <- 290

# --- 3. Graphique -----------------------------------------
ggplot(df, aes(x = annee)) +
    
    # Zones de phase (arrière-plan)
    annotate("rect", xmin = 2025, xmax = 2032, ymin = y_min, ymax = y_max,
             fill = "#FADBD8", alpha = 0.45) +
    annotate("rect", xmin = 2032, xmax = 2075, ymin = y_min, ymax = y_max,
             fill = "#D5F5E3", alpha = 0.40) +
    
    # Barres flux net actualisé
    geom_col(aes(y = flux_actu, fill = flux_sign),
             width = 0.7, alpha = 0.75) +
    scale_fill_manual(
        values = c("négatif" = "#E8A0A0", "positif" = "#82C882"),
        name   = NULL,
        labels = c("négatif" = "Flux net annuel actualisé",
                   "positif" = "Flux net annuel actualisé")
    ) +
    
    # Courbe VAN cumulée
    geom_line(aes(y = van_cum), color = "#1A3A5C", linewidth = 1.0) +
    geom_point(aes(y = van_cum), color = "#1A3A5C", size = 2.0) +
    
    # Ligne zéro
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    
    # Payback — ligne verticale pointillée
    geom_vline(xintercept = payback, linetype = "dashed",
               color = "gray50", linewidth = 0.8) +
    
    
    # Boîte VAN finale
    annotate("label", x = 2027, y = 245,
             label = paste0("VAN-SE finale = ", van_fin, " Mrd $"),
             fontface = "bold", size = 3.8, color = "#1A3A5C",
             fill = "white", label.size = 0.6) +
    
    # Axes et labels
    scale_x_continuous(breaks = seq(2025, 2075, 5), limits = c(2024.5, 2075)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(
        title    = "VAN cumulée socio-économique du projet Yarlung Tsangpo (2025–2074)",
        subtitle = "Taux d'actualisation 4,5 % — Quinet (2013)",
        x        = "Année",
        y        = "Mrd $ (constants 2025)"
    ) +
    
    # Légende manuelle pour les phases + courbe
    guides(fill = guide_legend(order = 2)) +
    
    theme_bw(base_size = 11) +
    theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
    )

# --- 4. Export --------------------------------------------
ggsave("van_yarlung_R.png", width = 13, height = 7, dpi = 180, bg = "white")
message("Graphique sauvegardé : van_yarlung_R.png")
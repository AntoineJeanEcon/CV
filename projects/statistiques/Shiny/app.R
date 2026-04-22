############################################################
## Dashboard Shiny – Réussite scolaire 
## (UI : intégration du thème style_text.css, contenu inchangé)
############################################################

library(shiny)
library(tidyverse)
library(scales)
library(gtsummary)
library(gt)
library(janitor)

#----------------------------#
# 1. Données
#----------------------------#

eleves <- read.csv("Mathes_D4.csv") |>
    clean_names()

eleves <- eleves |>
    mutate(
        pass = factor(pass, c("fail", "pass"), c("Échec", "Réussite")),
        genre = factor(sex, c("F", "M"), c("Fille", "Garçon")),
        romantic = factor(romantic, c("no", "yes"), c("Pas en couple", "En couple")),
        studytime = factor(
            studytime,
            levels = 1:4,
            labels = c("Moins de 2 h", "2–5 h", "5–10 h", "10 h et +"),
            ordered = TRUE
        ),
        absences = as.numeric(absences),
        failures = as.numeric(failures),
        g3 = as.numeric(g3),
        absences_classe = cut(
            absences,
            c(-1, 0, 4, 8, 14, Inf),
            labels = c("Aucune", "1 à 4", "5 à 8", "9 à 14", "Supérieure à 15")
        )
    )

palette_pass <- c("Échec" = "#E41A1C", "Réussite" = "#377EB8")

#############################
# 2. UI 
#############################

ui <- fluidPage(
    # --- Thème CSS (mettre style_text.css dans /www) ---
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style_text.css"),
        tags$title("Analyse de la réussite scolaire – Mathes_D4")
    ),
    
    # --- POPUP + effets (fonctionnalités prévues par style_text.css) ---
    # Le CSS anime automatiquement ces éléments (apparition/disparition).
    div(id = "welcome-popup", "🎉 Bonne année Monsieur PERON 🎉"),
    
    div(
        class = "fireworks-container",
        lapply(1:16, function(i) {
            div(
                class = "firework-particle",
                style = paste0(
                    "--tx:", sample(-120:120, 1), "px;",
                    "--ty:", sample(-120:120, 1), "px;"
                )
            )
        })
    ),
    
    lapply(1:12, function(i) {
        div(
            class = "confetti",
            style = paste0(
                "left:", sample(40:60, 1), "vw;",
                "--color: hsl(", sample(0:360, 1), ",90%,60%);"
            )
        )
    }),
    
    # On remplace titlePanel() pour profiter du style H1 du CSS (texte identique)
    h1("Analyse de la réussite scolaire – Mathes_D4"),
    
    tabsetPanel(
        
        tabPanel(
            "Introduction",
            div(
                class = "boite-texte",
                span(class = "badge", "Intro"),
                htmlOutput("intro_text")
            )
        ),
        
        tabPanel(
            "Vue d’ensemble",
            
            div(
                class = "boite-texte",
                span(class = "badge", "Filtre"),
                h4("Plage de notes"),
                sliderInput(
                    "g3_range",
                    label = NULL,
                    min = floor(min(eleves$g3, na.rm = TRUE)),
                    max = ceiling(max(eleves$g3, na.rm = TRUE)),
                    value = c(
                        floor(min(eleves$g3, na.rm = TRUE)),
                        ceiling(max(eleves$g3, na.rm = TRUE))
                    ),
                    step = 1
                )
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Distribution des notes"),
                plotOutput("dist_g3")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("dist_g3_text")
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Indicateurs clés"),
                gt_output("kpi_table")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("kpi_table_text")
            )
        ),
        
        tabPanel(
            "Temps d’étude",
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Notes selon le temps d’étude"),
                plotOutput("study_plot")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("study_plot_text")
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Taux de réussite par temps d’étude"),
                gt_output("study_table")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("study_table_text")
            )
        ),
        
        tabPanel(
            "Absences & Échecs",
            
            div(
                class = "boite-texte",
                span(class = "badge", "Choix"),
                h4("Variable affichée"),
                radioButtons(
                    "var_choice", NULL,
                    c("Absences" = "abs",
                      "Échecs antérieurs" = "fail"),
                    inline = TRUE
                )
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Proportions Réussite/Échec"),
                plotOutput("risk_plot")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("risk_plot_text")
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Tableau de risque"),
                gt_output("risk_table")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("risk_table_text")
            )
        ),
        
        tabPanel(
            "Facteurs personnels",
            
            div(
                class = "boite-texte",
                span(class = "badge", "Choix"),
                h4("Variable affichée"),
                radioButtons(
                    "perso_var", NULL,
                    c("Genre" = "genre",
                      "Situation amoureuse" = "romantic"),
                    inline = TRUE
                )
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Proportions Réussite/Échec"),
                plotOutput("perso_plot")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("perso_plot_text")
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Tableau par modalité"),
                gt_output("perso_table")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("perso_table_text")
            )
        ),
        
        tabPanel(
            "Synthèse & Décision",
            
            div(
                class = "boite-texte",
                span(class = "badge", "Message"),
                htmlOutput("key_message")
            ),
            
            div(
                class = "boite-graph",
                div(class = "boite-graph-title", "Régression logistique (univariée)"),
                gt_output("logit_table")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Lecture"),
                htmlOutput("logit_table_text")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Décision"),
                htmlOutput("decision_text")
            ),
            
            div(
                class = "boite-texte",
                span(class = "badge", "Reco"),
                htmlOutput("recommendations")
            )
        )
    )
)

############################################################
# 3. Server
############################################################

server <- function(input, output, session) {
    
    #################
    # Helpers 
    #################
    
    fmt_p <- function(p) {
        if (is.na(p)) return("NA")
        if (p < 0.001) return("&lt; 0,001")
        format(round(p, 3), nsmall = 3, decimal.mark = ",")
    }
    
    fmt_num <- function(x, digits = 2) {
        if (is.na(x)) return("NA")
        format(round(x, digits), nsmall = digits, decimal.mark = ",")
    }
    
    label_strength_v <- function(v) {
        if (is.na(v)) return("non estimable")
        if (v < 0.10) return("faible")
        if (v < 0.30) return("modérée")
        "marquée"
    }
    
    label_strength_eta2 <- function(e) {
        if (is.na(e)) return("non estimable")
        if (e < 0.01) return("très faible")
        if (e < 0.06) return("faible")
        if (e < 0.14) return("modérée")
        "marquée"
    }
    
    # Association x (qualitative) vs pass (qualitative)
    safe_assoc <- function(tbl) {
        if (is.null(dim(tbl)) || any(dim(tbl) < 2)) {
            return(list(ok = FALSE, p = NA_real_, stat = NA_real_, df = NA_integer_, v = NA_real_, method = "Test non calculable"))
        }
        
        tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
        if (any(dim(tbl) < 2)) {
            return(list(ok = FALSE, p = NA_real_, stat = NA_real_, df = NA_integer_, v = NA_real_, method = "Test non calculable"))
        }
        
        chi0 <- suppressWarnings(chisq.test(tbl, correct = FALSE))
        expected_low <- any(chi0$expected < 5)
        
        if (all(dim(tbl) == c(2, 2)) && expected_low) {
            ft <- suppressWarnings(fisher.test(tbl))
            n <- sum(tbl)
            v <- sqrt(as.numeric(chi0$statistic) / (n * (min(nrow(tbl) - 1, ncol(tbl) - 1))))
            return(list(ok = TRUE, p = as.numeric(ft$p.value), stat = as.numeric(chi0$statistic), df = 1L, v = v, method = "Fisher (p) + Phi/V (effet)"))
        }
        
        use_sim <- expected_low && (nrow(tbl) > 2 || ncol(tbl) > 2)
        chi <- if (use_sim) {
            suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE, B = 5000))
        } else {
            suppressWarnings(chisq.test(tbl, correct = all(dim(tbl) == c(2, 2))))
        }
        
        n <- sum(tbl)
        v <- sqrt(as.numeric(chi0$statistic) / (n * (min(nrow(tbl) - 1, ncol(tbl) - 1))))
        
        list(
            ok = TRUE,
            p = as.numeric(chi$p.value),
            stat = as.numeric(chi0$statistic),
            df = as.integer(chi0$parameter),
            v = v,
            method = if (use_sim) "Chi-deux (p simulée)" else "Chi-deux"
        )
    }
    
    # ANOVA + eta² + Kruskal (robustesse) pour g3 ~ studytime
    safe_anova_kw <- function(df) {
        d <- df |> filter(!is.na(studytime), !is.na(g3)) |> droplevels()
        if (n_distinct(d$studytime) < 2) {
            return(list(ok = FALSE, p_anova = NA_real_, eta2 = NA_real_, p_kw = NA_real_))
        }
        
        fit <- aov(g3 ~ studytime, data = d)
        a <- summary(fit)[[1]]
        p_anova <- a[["Pr(>F)"]][1]
        ss_between <- a[["Sum Sq"]][1]
        ss_total <- sum(a[["Sum Sq"]], na.rm = TRUE)
        eta2 <- ss_between / ss_total
        
        kw <- kruskal.test(g3 ~ studytime, data = d)
        
        list(ok = TRUE, p_anova = as.numeric(p_anova), eta2 = as.numeric(eta2), p_kw = as.numeric(kw$p.value))
    }
    
    # IC95% pour une proportion
    prop_ci <- function(k, n) {
        ci <- suppressWarnings(prop.test(k, n)$conf.int)
        c(ci[1], ci[2])
    }
    
    # Génère une ligne “Verdict” courte
    verdict_assoc_line <- function(res, label) {
        if (!isTRUE(res$ok)) return(paste0("Verdict : test non disponible pour <strong>", label, "</strong> sur la sélection actuelle."))
        paste0(
            "Verdict : association <strong>", label_strength_v(res$v), "</strong> entre <strong>", label,
            "</strong> et la réussite (calculé sur la sélection actuelle)."
        )
    }
    
    verdict_eta_line <- function(tests) {
        if (!isTRUE(tests$ok)) return("Verdict : test non disponible sur la sélection actuelle.")
        paste0(
            "Verdict : effet <strong>", label_strength_eta2(tests$eta2),
            "</strong> du temps d’étude sur les notes (calculé sur la sélection actuelle)."
        )
    }
    
    #----------------------------#
    # Données filtrées par plage de notes G3
    #----------------------------#
    
    eleves_filtre <- reactive({
        req(input$g3_range)
        eleves |>
            filter(g3 >= input$g3_range[1],
                   g3 <= input$g3_range[2])
    })
    
    #----------------------------#
    # INTRODUCTION
    #----------------------------#
    
    output$intro_text <- renderUI({
        HTML("
    <p>
      Ce dashboard présente une analyse des <strong>facteurs associés à la réussite scolaire en mathématiques</strong>
      à partir de la base <em>Mathes_D4</em>. L’objectif est de comprendre <em>quels profils se distinguent</em> (réussite vs échec)
      et <em>quels signaux</em> aident à interpréter les écarts observés.
    </p>
    <p>
      La lecture suit une progression : (1) niveau global et dispersion des notes, (2) temps d’étude,
      (3) signaux de risque (absences, échecs antérieurs), (4) facteurs personnels pour contextualiser.
      Les résultats décrivent des <strong>associations</strong> observées dans les données ; ils n’impliquent pas une relation causale.
    </p>
    ")
    })
    
    #----------------------------#
    # VUE D’ENSEMBLE
    #----------------------------#
    
    output$dist_g3 <- renderPlot({
        ggplot(eleves_filtre(), aes(x = g3)) +
            geom_histogram(bins = 20, fill = "#1F78B4") +
            labs(
                x = "Note",
                y = "Effectif"
            ) +
            theme_minimal()
    })
    
    output$dist_g3_text <- renderUI({
        HTML("
    <p>
      <strong>Commentaire :</strong> la distribution des notes indique si la performance est globalement homogène
      ou si elle se structure en sous-groupes (par exemple un noyau d’élèves en difficulté). Cette vue d’ensemble
      sert de point de départ : on cherche ensuite <em>quels facteurs</em> différencient le plus les profils.
    </p>
    ")
    })
    
    output$kpi_table <- render_gt({
        df <- eleves_filtre()
        df |>
            summarise(
                `Note moyenne` = mean(g3, na.rm = TRUE),
                `Taux de réussite (%)` = mean(pass == "Réussite") * 100,
                `Élèves à risque (%)` = mean(failures >= 1) * 100
            ) |>
            gt() |>
            fmt_number(everything(), decimals = 1)
    })
    
    # Option A+ : texte simple + details = verdict + chiffres
    output$kpi_table_text <- renderUI({
        df <- eleves_filtre()
        n <- nrow(df)
        k <- sum(df$pass == "Réussite", na.rm = TRUE)
        tx_reussite <- round(k / n * 100, 1)
        tx_risque <- round(mean(df$failures >= 1) * 100, 1)
        
        ci <- prop_ci(k, n)
        ci_low <- round(ci[1] * 100, 1)
        ci_high <- round(ci[2] * 100, 1)
        
        msg <- paste0("
      <p>
        <strong>Lecture :</strong> sur la sélection actuelle, la réussite est de <strong>",
                      format(tx_reussite, nsmall = 1, decimal.mark = ","), "%</strong>.
        La part d’élèves avec au moins un échec antérieur est de <strong>",
                      format(tx_risque, nsmall = 1, decimal.mark = ","), "%</strong>,
        ce qui aide à apprécier le poids des profils potentiellement fragiles dans la cohorte.
      </p>
    ")
        
        det <- paste0(
            "<details><summary><em>Voir justification statistique</em></summary>",
            "<p><strong>", "Verdict : estimation précise du taux global (calculée sur la sélection actuelle).", "</strong></p>",
            "<p>IC95% du taux de réussite : <strong>",
            format(ci_low, nsmall = 1, decimal.mark = ","), "%</strong> à <strong>",
            format(ci_high, nsmall = 1, decimal.mark = ","), "%</strong> (n = ", n, ").</p>",
            "</details>"
        )
        
        HTML(paste0(msg, det))
    })
    
    #----------------------------#
    # TEMPS D’ÉTUDE
    #----------------------------#
    
    output$study_plot <- renderPlot({
        ggplot(eleves_filtre(), aes(x = studytime, y = g3)) +
            geom_boxplot(fill = "#1F78B4") +
            labs(
                x = "Temps d’étude",
                y = "Note"
            ) +
            theme_minimal()
    })
    
    output$study_plot_text <- renderUI({
        df <- eleves_filtre()
        tests <- safe_anova_kw(df)
        
        msg <- "
      <p>
        <strong>Commentaire :</strong> les boxplots montrent si l’augmentation du temps d’étude s’accompagne
        d’un déplacement global des notes (médiane plus haute) et/ou d’une dispersion plus faible.
        Quand les distributions se recouvrent fortement, le temps d’étude joue un rôle mais n’explique pas
        à lui seul les écarts : la régularité et la méthode de travail deviennent alors déterminantes.
      </p>
      <p>
        <strong>Remarque :</strong> le temps d’étude est <em>auto-déclaré</em>, donc potentiellement biaisé.
      </p>
    "
        
        det <- if (!tests$ok) {
            "<details><summary><em>Voir justification statistique</em></summary>
        <p><strong>Verdict : test non disponible sur la sélection actuelle.</strong></p>
        <p>Tests non calculables (modalités insuffisantes ou trop peu représentées).</p>
      </details>"
        } else {
            paste0(
                "<details><summary><em>Voir justification statistique</em></summary>",
                "<p><strong>", verdict_eta_line(tests), "</strong></p>",
                "<p>ANOVA : p = <strong>", fmt_p(tests$p_anova),
                "</strong> ; taille d’effet &eta;<sup>2</sup> = <strong>", fmt_num(tests$eta2, 3), "</strong>.</p>",
                "<p>Robustesse (Kruskal-Wallis) : p = <strong>", fmt_p(tests$p_kw), "</strong>.</p>",
                "</details>"
            )
        }
        
        HTML(paste0(msg, det))
    })
    
    output$study_table <- render_gt({
        eleves_filtre() |>
            group_by(studytime) |>
            summarise(
                Effectif = n(),
                `Taux de réussite (%)` = mean(pass == "Réussite") * 100
            ) |>
            arrange(`Taux de réussite (%)`) |>
            gt() |>
            cols_label(studytime = "Temps d’étude") |>
            fmt_number(`Taux de réussite (%)`, decimals = 1)
    })
    
    output$study_table_text <- renderUI({
        HTML("
    <p>
      <strong>Tableau :</strong> il complète le graphique en indiquant la taille de chaque groupe. Cela permet
      d’éviter de surinterpréter une modalité peu représentée et d’identifier les catégories où un changement
      de performance aurait le plus d’impact au niveau global.
    </p>
    ")
    })
    
    #----------------------------#
    # ABSENCES & ÉCHECS
    #----------------------------#
    
    output$risk_plot <- renderPlot({
        df <- eleves_filtre()
        
        if (input$var_choice == "abs") {
            df |>
                group_by(absences_classe, pass) |>
                summarise(n = n(), .groups = "drop") |>
                group_by(absences_classe) |>
                mutate(freq = n / sum(n)) |>
                ggplot(aes(x = absences_classe, y = freq, fill = pass)) +
                geom_col(position = "fill") +
                scale_y_continuous(labels = percent) +
                scale_fill_manual(values = palette_pass, name = "Résultat") +
                labs(
                    x = "Classe d’absences",
                    y = "Proportion"
                ) +
                theme_minimal()
        } else {
            df |>
                group_by(failures, pass) |>
                summarise(n = n(), .groups = "drop") |>
                group_by(failures) |>
                mutate(freq = n / sum(n)) |>
                ggplot(aes(x = factor(failures), y = freq, fill = pass)) +
                geom_col(position = "fill") +
                scale_y_continuous(labels = percent) +
                scale_fill_manual(values = palette_pass, name = "Résultat") +
                labs(
                    x = "Nombre d’échecs antérieurs",
                    y = "Proportion"
                ) +
                theme_minimal()
        }
    })
    
    output$risk_plot_text <- renderUI({
        df <- eleves_filtre()
        
        if (input$var_choice == "abs") {
            tbl <- with(df, table(droplevels(absences_classe), droplevels(pass)))
            res <- safe_assoc(tbl)
            
            msg <- "
        <p>
          <strong>Commentaire (Absences) :</strong> la lecture en proportions sert à repérer si l’absentéisme devient un <em>signal discriminant</em> aux niveaux les plus élevés. Si la part d’échec se concentre dans les classes “9 à 14 /Supérieure à 15”, l’assiduité joue surtout comme un marqueur d’alerte plutôt qu’un effet linéaire.
        </p>
      "
            
            det <- if (!res$ok) {
                "<details><summary><em>Voir justification statistique</em></summary>
          <p><strong>Verdict : test non disponible sur la sélection actuelle.</strong></p>
          <p>Test non calculable sur la sélection actuelle.</p>
        </details>"
            } else {
                paste0(
                    "<details><summary><em>Voir justification statistique</em></summary>",
                    "<p><strong>", verdict_assoc_line(res, "l’absentéisme"), "</strong></p>",
                    "<p>", res$method, " : p = <strong>", fmt_p(res$p),
                    "</strong> ; V de Cramer = <strong>", fmt_num(res$v, 3), "</strong>.</p>",
                    "</details>"
                )
            }
            
            HTML(paste0(msg, det))
        } else {
            f <- factor(df$failures)
            tbl <- table(droplevels(f), droplevels(df$pass))
            res <- safe_assoc(tbl)
            
            msg <- "
        <p>
          <strong>Commentaire (Échecs antérieurs) :</strong> un gradient (plus d’échecs → plus d’échec actuel)
          renvoie à une logique de <em>difficultés cumulées</em>. Ce facteur structure la segmentation des résultats
          et contribue à expliquer pourquoi certains élèves restent durablement en difficulté.
        </p>
      "
            
            det <- if (!res$ok) {
                "<details><summary><em>Voir justification statistique</em></summary>
          <p><strong>Verdict : test non disponible sur la sélection actuelle.</strong></p>
          <p>Test non calculable sur la sélection actuelle.</p>
        </details>"
            } else {
                paste0(
                    "<details><summary><em>Voir justification statistique</em></summary>",
                    "<p><strong>", verdict_assoc_line(res, "les échecs antérieurs"), "</strong></p>",
                    "<p>", res$method, " : p = <strong>", fmt_p(res$p),
                    "</strong> ; V de Cramer = <strong>", fmt_num(res$v, 3), "</strong>.</p>",
                    "</details>"
                )
            }
            
            HTML(paste0(msg, det))
        }
    })
    
    output$risk_table <- render_gt({
        df <- eleves_filtre()
        
        if (input$var_choice == "abs") {
            df |>
                group_by(absences_classe) |>
                summarise(
                    Effectif = n(),
                    `Taux de réussite (%)` = mean(pass == "Réussite") * 100
                ) |>
                arrange(`Taux de réussite (%)`) |>
                gt() |>
                cols_label(absences_classe = "Classe d’absences") |>
                fmt_number(`Taux de réussite (%)`, decimals = 1)
        } else {
            df |>
                group_by(failures) |>
                summarise(
                    Effectif = n(),
                    `Taux de réussite (%)` = mean(pass == "Réussite") * 100
                ) |>
                arrange(`Taux de réussite (%)`) |>
                gt() |>
                cols_label(failures = "Nombre d’échecs antérieurs") |>
                fmt_number(`Taux de réussite (%)`, decimals = 1)
        }
    })
    
    output$risk_table_text <- renderUI({
        if (input$var_choice == "abs") {
            HTML("
      <p>
        <strong>Tableau (Absences) :</strong> il met en regard le niveau de risque (taux de réussite) et la taille des groupes
        (effectifs). Cette double lecture évite de conclure uniquement à partir du groupe “le plus à risque” si celui-ci est très minoritaire.
      </p>
      ")
        } else {
            HTML("
      <p>
        <strong>Tableau (Échecs antérieurs) :</strong> il quantifie la dégradation des taux de réussite selon l’historique d’échecs
        et donne les effectifs associés. Il aide à mesurer quels profils pèsent le plus sur la réussite globale.
      </p>
      ")
        }
    })
    
    #----------------------------#
    # FACTEURS PERSONNELS
    #----------------------------#
    
    output$perso_plot <- renderPlot({
        df <- eleves_filtre()
        var <- input$perso_var
        
        df |>
            group_by(.data[[var]], pass) |>
            summarise(n = n(), .groups = "drop") |>
            group_by(.data[[var]]) |>
            mutate(freq = n / sum(n)) |>
            ggplot(aes(x = .data[[var]], y = freq, fill = pass)) +
            geom_col(position = "fill") +
            scale_y_continuous(labels = percent) +
            scale_fill_manual(values = palette_pass, name = "Résultat") +
            labs(
                x = if (var == "genre") "Genre" else "Situation amoureuse",
                y = "Proportion"
            ) +
            theme_minimal()
    })
    
    output$perso_plot_text <- renderUI({
        df <- eleves_filtre()
        var <- input$perso_var
        
        if (var == "genre") {
            tbl <- table(droplevels(df$genre), droplevels(df$pass))
            res <- safe_assoc(tbl)
            
            msg <- "
        <p>
          <strong>Commentaire (Genre) :</strong> l’objectif est de vérifier si le genre distingue réellement la réussite dans cette base.
          Un écart faible suggère un facteur surtout contextuel, à remettre en perspective face aux leviers scolaires.
        </p>
      "
            
            det <- if (!res$ok) {
                "<details><summary><em>Voir justification statistique</em></summary>
          <p><strong>Verdict : test non disponible sur la sélection actuelle.</strong></p>
          <p>Test non calculable sur la sélection actuelle.</p>
        </details>"
            } else {
                paste0(
                    "<details><summary><em>Voir justification statistique</em></summary>",
                    "<p><strong>", verdict_assoc_line(res, "le genre"), "</strong></p>",
                    "<p>", res$method, " : p = <strong>", fmt_p(res$p),
                    "</strong> ; V de Cramer = <strong>", fmt_num(res$v, 3), "</strong>.</p>",
                    "</details>"
                )
            }
            
            HTML(paste0(msg, det))
        } else {
            tbl <- table(droplevels(df$romantic), droplevels(df$pass))
            res <- safe_assoc(tbl)
            
            msg <- "
        <p>
          <strong>Commentaire (Situation amoureuse) :</strong> on vérifie si la différence observée est suffisamment structurante
          pour éclairer la réussite. Dans beaucoup de contextes, ce facteur reste secondaire comparé à l’historique scolaire et à l’assiduité.
        </p>
      "
            
            det <- if (!res$ok) {
                "<details><summary><em>Voir justification statistique</em></summary>
          <p><strong>Verdict : test non disponible sur la sélection actuelle.</strong></p>
          <p>Test non calculable sur la sélection actuelle.</p>
        </details>"
            } else {
                paste0(
                    "<details><summary><em>Voir justification statistique</em></summary>",
                    "<p><strong>", verdict_assoc_line(res, "la situation amoureuse"), "</strong></p>",
                    "<p>", res$method, " : p = <strong>", fmt_p(res$p),
                    "</strong> ; V de Cramer = <strong>", fmt_num(res$v, 3), "</strong>.</p>",
                    "</details>"
                )
            }
            
            HTML(paste0(msg, det))
        }
    })
    
    output$perso_table <- render_gt({
        df <- eleves_filtre()
        var <- input$perso_var
        
        tab <- df |>
            group_by(.data[[var]]) |>
            summarise(
                Effectif = n(),
                `Taux de réussite (%)` = mean(pass == "Réussite") * 100
            ) |>
            arrange(`Taux de réussite (%)`)
        
        gt(tab) |>
            cols_label(!!var := if (var == "genre") "Genre" else "Situation amoureuse") |>
            fmt_number(`Taux de réussite (%)`, decimals = 1)
    })
    
    output$perso_table_text <- renderUI({
        if (input$perso_var == "genre") {
            HTML("
      <p>
        <strong>Tableau (Genre) :</strong> il précise les effectifs et les taux de réussite afin de contextualiser l’écart
        et d’éviter une conclusion basée uniquement sur l’aspect visuel.
      </p>
      ")
        } else {
            HTML("
      <p>
        <strong>Tableau (Situation amoureuse) :</strong> il indique la taille des groupes et les taux associés, utile pour juger
        si un écart est stable et interprétable.
      </p>
      ")
        }
    })
    
    #----------------------------#
    # SYNTHÈSE & DÉCISION
    #----------------------------#
    
    output$key_message <- renderUI({
        HTML("
    <h3><strong>
      La réussite scolaire dépend avant tout des difficultés accumulées dans le parcours antérieur.
    </strong></h3>
    ")
    })
    
    output$logit_table <- render_gt({
        df <- eleves_filtre() |>
            mutate(failures_f = factor(failures))
        
        df |>
            select(pass, studytime, absences_classe, failures_f, genre, romantic) |>
            tbl_uvregression(
                method = glm,
                y = pass,
                method.args = list(family = binomial),
                exponentiate = TRUE
            ) |>
            as_gt()
    })
    
    output$logit_table_text <- renderUI({
        msg <- "
      <p>
        <strong>Tableau récapitulatif :</strong> il compare, facteur par facteur, l’association avec la probabilité de réussite (OR).
        Il sert à hiérarchiser les variables qui distinguent le plus les profils, tout en gardant une lecture prudente :
        une association n’implique pas nécessairement un lien causal.
      </p>
    "
        det <- "
      <details><summary><em>Voir une lecture simple du tableau</em></summary>
      <p><strong>Verdict : l’OR indique la direction de l’association (sur la sélection actuelle), l’IC donne sa précision.</strong></p>
      <p>OR &lt; 1 : associé à une probabilité plus faible de réussite ; OR &gt; 1 : associé à une probabilité plus élevée.</p>
      </details>
    "
        HTML(paste0(msg, det))
    })
    
    output$decision_text <- renderUI({
        HTML("
    <p><strong>Conseils & décisions :</strong></p>
    <p>
      Les résultats mettent davantage en évidence des associations avec l’historique d’échecs et l’assiduité qu’avec les facteurs personnels.
      La décision finale (priorisation, seuils d’alerte, modalités de suivi) revient au lecteur et doit intégrer le contexte pédagogique.
    </p>
    ")
    })
    
    output$recommendations <- renderUI({
        HTML("
    <ul>
      <li><strong>Repérage :</strong> documenter les profils avec échecs antérieurs et suivre leur trajectoire.</li>
      <li><strong>Assiduité :</strong> surveiller les classes d’absences élevées et vérifier si elles concentrent l’échec.</li>
      <li><strong>Pratiques d’étude :</strong> accompagner la régularité et la méthode (le temps d’étude seul n’explique pas toute la variabilité).</li>
    </ul>
    ")
    })
}

shinyApp(ui, server)
plot_resultats <- function(res,
                           nb_sim,
                           nb_bv_tires,
                           nb_max_bulletins_tires,
                           lang = "fr") {
  
  # -----------------------------
  # Gestion de la langue
  # -----------------------------
  if (lang == "eng") {
    txt <- list(
      title = paste0(
        "Distribution of estimates (",
        "Monte Carlo simulations with ", nb_sim,
        " draws\n of at most ", nb_max_bulletins_tires,
        " ballots in ", nb_bv_tires, " polling stations)"
      ),
      subtitle = paste0(
        "Solid lines: true values | ",
        "Long dashes (--): sampling frame values | \n",
        "Medium dashes (- -): metropolitan France results | ",
        "Dotted lines: polling institutes estimates at 8pm"
      ),
      x = "\nSampling design",
      y = "Estimate (%)\n",
      fill = "Candidate"
    )
  } else {
    txt <- list(
      title = paste0(
        "Distribution des estimations (",
        "simulations Monte-Carlo avec ", nb_sim,
        " tirages de maximum ", nb_max_bulletins_tires,
        " bulletins dans ", nb_bv_tires, " bureaux de vote)"
      ),
      subtitle = paste0(
        "Traits pleins : valeurs réelles | ",
        "Longs tirets (--) : valeurs base de sondage | ",
        "Tirets moyens (- -) : résultats France métropolitaine | ",
        "Pointillés : estimations des instituts à 20h"
      ),
      x = "\nPlan de sondage",
      y = "Estimation (%)\n",
      fill = "Candidat"
    )
  }
  
  # -----------------------------
  # Mise en forme des méthodes
  # -----------------------------
  niveaux_methodes <- c(
    "simple",
    "simple_cale",
    "stratfilosofi_cale",
    "stratfilosofi2017_cale",
    "cube_filosofi2017_cale",
    "cubestrat_filosofi2017_cale"
  )
  
  res_resultat <- res %>%
    mutate(methode = factor(methode, levels = niveaux_methodes))
  
  # -----------------------------
  # Valeurs réelles (résultats officiels)
  # -----------------------------
  valeurs_reelles <- tibble(
    candidat = c("MACRON", "LEPEN", "MELENCHON"),
    valeur = c(
      valeur_reelle(NULL, "MACRON", "T1"),
      valeur_reelle(NULL, "LEPEN", "T1"),
      valeur_reelle(NULL, "MELENCHON", "T1")
    ),
    type_ligne = "Valeurs réelles"
  )
  
  # -----------------------------
  # Valeurs de la base de sondage
  # -----------------------------
  valeurs_base_sondage <- tibble(
    candidat = c("MACRON", "LEPEN", "MELENCHON"),
    valeur = c(
      valeur_reelle_base_sondage(NULL, "MACRON", "T1"),
      valeur_reelle_base_sondage(NULL, "LEPEN", "T1"),
      valeur_reelle_base_sondage(NULL, "MELENCHON", "T1")
    ),
    type_ligne = "Base de sondage"
  )
  
  # -----------------------------
  # Résultats France métropolitaine
  # -----------------------------
  valeurs_france_metro <- tibble(
    candidat    = c("MACRON", "LEPEN", "MELENCHON"),
    valeur      = c(27.77, 23.46, 21.51),
    type_ligne  = "France métropolitaine"
  )
  
  # -----------------------------
  # Estimations instituts
  # -----------------------------
  sondages_instituts <- tibble(
    institut = rep(
      c("TF1/Ifop", "FTV/Ipsos", "M6/Harris", "BFMTV/Elabe", "CNEWS/OpinionWay"),
      3
    ),
    candidat = rep(c("MACRON", "LEPEN", "MELENCHON"), each = 5),
    valeur = c(
      28.6, 28.1, 28.3, 28.5, 29.0,
      24.4, 23.3, 24.9, 24.2, 24.0,
      20.2, 20.1, 20.0, 20.2, 20.0
    )
  )
  
  # -----------------------------
  # Palette de couleurs
  # -----------------------------
  palette_couleurs <- c(
    "LEPEN"     = "#E76F51",
    "MACRON"    = "#2A9D8F",
    "MELENCHON" = "#457B9D"
  )
  
  
  # -----------------------------
  # Graphique
  # -----------------------------
  ggplot() +
    
    # Boîtes à moustaches
    geom_boxplot(
      data = res_resultat,
      aes(x = methode, y = estimation, fill = candidat),
      position = position_dodge2(width = 0.8, padding = 0.35),
      width = 0.6,
      alpha = 0.9,
      linewidth = 0.5,
      outlier.shape = NA
    ) +
    
    # Valeurs réelles (traits pleins)
    geom_hline(
      data = valeurs_reelles,
      aes(yintercept = valeur, color = candidat),
      linetype = "solid",
      linewidth = 1.1
    ) +
    
    # Valeurs base de sondage (longs tirets)
    geom_hline(
      data = valeurs_base_sondage,
      aes(yintercept = valeur, color = candidat),
      linetype = "longdash",
      linewidth = 0.9,
      alpha = 0.85
    ) +
    
    # Résultats France métropolitaine (tirets moyens)
    geom_hline(
      data = valeurs_france_metro,
      aes(yintercept = valeur, color = candidat),
      linetype = "dashed",
      linewidth = 0.9,
      alpha = 0.85
    ) +
    
    # Estimations instituts (pointillés)
    geom_hline(
      data = sondages_instituts,
      aes(yintercept = valeur, color = candidat),
      linetype = "dotted",
      linewidth = 0.8,
      alpha = 0.7
    ) +
    
    # Étiquettes instituts
    geom_text_repel(
      data = sondages_instituts,
      aes(
        x = length(niveaux_methodes) + 0.7,
        y = valeur,
        label = institut,
        color = candidat
      ),
      hjust = 0,
      size = 2.6,
      direction = "y",
      segment.size = 0.25,
      segment.alpha = 0.5,
      show.legend = FALSE
    ) +
    
    # -----------------------------
  # Échelles
  # -----------------------------
  scale_fill_manual(
    values = palette_couleurs
  ) +
    scale_color_manual(
      values = palette_couleurs,
      guide = "none"
    ) +
    
    scale_y_continuous(
      breaks = seq(18, 30, by = 1),
      minor_breaks = NULL
    ) +
    scale_x_discrete(
      expand = expansion(add = c(0.6, 1.4))
    ) +
    
    labs(
      title    = txt$title,
      subtitle = txt$subtitle,
      x        = txt$x,
      y        = txt$y,
      fill     = txt$fill
    ) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_size = 11) +
    theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
      axis.text.x  = element_text(angle = 30, hjust = 1, size = 9),
      axis.title   = element_text(face = "bold"),
      plot.title   = element_text(face = "bold", size = 13, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 9, color = "grey30", margin = margin(b = 10)),
      plot.margin  = margin(10, 10, 10, 10)
    )
}
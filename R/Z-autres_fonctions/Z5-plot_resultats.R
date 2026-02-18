plot_resultats <- function(res) {
  
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(tibble)
  
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
  # Fonds alternés (sans légende)
  # -----------------------------
  fond_methodes <- tibble(
    xmin = seq_along(niveaux_methodes) - 0.5,
    xmax = seq_along(niveaux_methodes) + 0.5,
    fond = rep(c(TRUE, FALSE), length.out = length(niveaux_methodes))
  )
  
  # -----------------------------
  # Valeurs réelles
  # -----------------------------
  valeurs_reelles <- tibble(
    candidat = c("MACRON", "LEPEN", "MELENCHON"),
    valeur = c(
      valeur_reelle(NULL, "MACRON", "T1"),
      valeur_reelle(NULL, "LEPEN", "T1"),
      valeur_reelle(NULL, "MELENCHON", "T1")
    )
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
  # Graphique
  # -----------------------------
  ggplot() +
    
    # Fonds alternés discrets
    geom_rect(
      data = fond_methodes,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = "grey70",
      alpha = 0.04
    ) +
    
    # Boxplots par 3
    geom_boxplot(
      data = res_resultat,
      aes(
        x = methode,
        y = estimation,
        fill = candidat
      ),
      position = position_dodge2(
        width = 0.8,
        padding = 0.35
      ),
      width = 0.6,
      alpha = 0.9,
      linewidth = 0.5,
      outlier.shape = NA
    ) +
    
    # Valeurs réelles
    geom_hline(
      data = valeurs_reelles,
      aes(yintercept = valeur, color = candidat),
      linewidth = 1.1
    ) +
    
    # Instituts (pointillés)
    geom_hline(
      data = sondages_instituts,
      aes(yintercept = valeur, color = candidat),
      linetype = "dotted",
      linewidth = 0.8,
      alpha = 0.7
    ) +
    
    # Labels instituts (plus à droite)
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
    
    # Couleurs
    scale_fill_manual(
      values = c(
        "LEPEN" = "#E76F51",
        "MACRON" = "#2A9D8F",
        "MELENCHON" = "#457B9D"
      )
    ) +
    scale_color_manual(
      values = c(
        "LEPEN" = "#E76F51",
        "MACRON" = "#2A9D8F",
        "MELENCHON" = "#457B9D"
      ),
      guide = "none"
    ) +
    
    # Axes
    scale_y_continuous(
      breaks = seq(18, 30, by = 1),
      minor_breaks = NULL
    ) +
    scale_x_discrete(
      expand = expansion(add = c(0.6, 1.4))
    ) +
    
    # Titres
    labs(
      title = paste0(
        "Distribution des estimations (",
        "simulations Monte-Carlo avec ", nb_sim,
        " tirages de maximum ", nb_max_bulletins_tires,
        " bulletins dans ", nb_bv_tires, " bureaux de vote)"
      ),
      subtitle = "Traits pleins : valeurs réelles | Pointillés : estimations des instituts à 20h",
      x = "\nPlan de sondage",
      y = "Estimation (%)\n",
      fill = ""
    ) +
    
    coord_cartesian(clip = "off") +
    
    # Thème
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
      
      axis.text.x = element_text(
        angle = 30,
        hjust = 1,
        size = 9
      ),
      
      axis.title = element_text(face = "bold"),
      
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 10,
        color = "grey30",
        margin = margin(b = 10)
      ),
      
      plot.margin = margin(10, 10, 10, 10)
    )
}

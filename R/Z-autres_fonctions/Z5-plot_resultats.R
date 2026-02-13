plot_resultats <- function(res) {
  
  res_resultat <- res %>% 
    mutate(methode = ifelse(calage, paste0(methode, "_cale"), methode),
           methode = factor(methode, 
                            levels = c("simple", "simple_cale",
                                       "stratfilosofi_cale",
                                       "stratfilosofi2017_cale",
                                       "cubestrat")))
  
  # Calculer les valeurs réelles pour chaque candidat
  valeurs_reelles <- data.frame(
    candidat = c("MACRON", "LEPEN", "MELENCHON"),
    valeur = c(
      valeur_reelle(NULL, "MACRON", NULL, "T1"),
      valeur_reelle(NULL, "LEPEN", NULL, "T1"),
      valeur_reelle(NULL, "MELENCHON", NULL, "T1")
    )
  )
  
  # Estimations des instituts de sondage
  sondages_instituts <- data.frame(
    institut = rep(c("TF1/Ifop", "FTV/Ipsos", "M6/Harris", "BFMTV/Elabe", "CNEWS/OpinionWay"), 3),
    candidat = rep(c("MACRON", "LEPEN", "MELENCHON"), each = 5),
    valeur = c(
      # MACRON
      28.6, 28.1, 28.3, 28.5, 29,
      # LEPEN
      24.4, 23.3, 24.9, 24.2, 24,
      # MELENCHON
      20.2, 20.1, 20, 20.2, 20
    )
  )
  
  # Graphique avec les lignes horizontales
  ggplot(res_resultat, aes(x=methode, y=estimation, fill=candidat)) + 
    geom_boxplot() +
    # Valeurs réelles (trait plein)
    geom_hline(data = valeurs_reelles, 
               aes(yintercept = valeur, color = candidat),
               linetype = "solid",
               linewidth = 0.8) +
    # Estimations instituts (pointillés)
    geom_hline(data = sondages_instituts, 
               aes(yintercept = valeur, color = candidat),
               linetype = "dotted",
               linewidth = 0.8,
               alpha = 0.7) +
    # Labels des instituts à droite avec ggrepel
    geom_text_repel(data = sondages_instituts,
                    aes(x = Inf, y = valeur, label = institut, color = candidat),
                    hjust = 0,
                    size = 2.5,
                    direction = "y",
                    xlim = c(5.4, Inf),
                    segment.size = 0.2,
                    segment.alpha = 0.5) +
    scale_color_manual(values = c("LEPEN" = "#F8766D", 
                                  "MACRON" = "#00BA38", 
                                  "MELENCHON" = "#619CFF"),
                       guide = "none") +
    scale_y_continuous(breaks = seq(18, 30, by = 1),
                       minor_breaks = seq(18, 30, by = 0.2)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.3))) +
    labs(
      title = paste0(
        "Distribution des estimations (",
        "simulations Monte-Carlo avec ", nb_sim, " tirages de maximum ",
        nb_max_bulletins_tires, " bulletins dans ", nb_bv_tires,
        " bureaux de vote)"),
      subtitle = "Lignes pleines : valeurs réelles dans la base des bureaux de vote du projet | Lignes pointillées : estimations des instituts de sondage à 20h",
      x = "\nPlan de sondage",
      y = "Estimation (%)\n",
      fill = ""
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      plot.margin = margin(5.5, 80, 5.5, 5.5),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray85", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray92", linewidth = 0.3),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "gray30")
    )
  
}
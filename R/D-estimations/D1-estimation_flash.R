estimation_flash <- function(ech, candidat, methode, tour = "T1") {
  
  # Calcul de l'estimation en pourcentage
  estimation_pct <- round(
    (sum(ech[[candidat]] * ech[[paste0("poids_", methode)]]) /
       sum(ech[[paste0("poids_", methode)]])) * 100, 2
  )
  
  # Valeur réelle en pourcentage
  valeur_reelle_pct <- round(
    (sum(bv_2022_final[[paste0(candidat, "_", tour)]]) / sum(
      bv_2022_final[[paste0("EXPRIMES_", tour)]]) * 100
    ), 2)
  
  # Nombre total de bulletins et de bureaux de vote
  nb_bulletins <- nrow(ech)
  nb_bureaux <- length(unique(ech$ID))
  
  # Message de synthèse
  message <- paste0(
    candidat, " | ",
    tour, " | ",
    "Méthode : ", methode, " | ",
    "Estimation : ", estimation_pct, " % (", nb_bulletins, " bulletins, dans ",
    nb_bureaux, " bureaux de vote) | ",
    "Valeur réelle : ", valeur_reelle_pct, " %"
  )
  
  # Affichage du message
  cat(message, "\n")

}

estimation_flash <- function(ech, candidat, tour = "T1") {
  
  methode <- deparse(substitute(ech))
  
  # Calcul de l'estimation en pourcentage
  estimation_pct <- estimation_brute(ech, candidat)
  
  # Valeur réelle en pourcentage
  valeur_reelle_pct <- valeur_reelle(ech, candidat, tour)
  
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

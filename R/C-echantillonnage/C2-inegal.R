tirage_autopondere <- function(nb_bv_tires, nb_max_bulletins_tires,
                               poids_cales, tour = "T1") {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # tour <- "T1"

  bdd_autopondere <- base_sondage %>% 
    filter(
      base_sondage[[paste0("INSCRITS_", tour)]] >= nb_max_bulletins_tires * 1.5
    )
  
  base_sondage$proba_inegal_d1 <- 
    sampling::inclusionprobabilities(
      base_sondage[[paste0("INSCRITS_", tour)]],
      n = nb_bv_tires)
  
  indic_d1 <- sampling::UPsystematic(base_sondage$proba_inegal_d1)
  
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = indic_d1,
                          tour = tour,
                          methode = "autopondere",
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales))
  
}

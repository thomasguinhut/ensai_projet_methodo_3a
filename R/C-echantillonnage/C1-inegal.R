tirage_inegal <- function(nb_bv_tires, nb_max_bulletins_tires,
                          autopondere, tour = "T1") {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # autopondere <- TRUE
  # tour <- "T1"
  
  if(autopondere) {
    base_sondage <- base_sondage %>% 
      filter(
        base_sondage[[paste0("EXPRIMES_", tour)]] >= nb_max_bulletins_tires
      )
  }
  
  base_sondage$proba_inegal_d1 <- 
    sampling::inclusionprobabilities(
      base_sondage[[paste0("INSCRITS_", tour)]],
      n = nb_bv_tires)
  
  indic_d1 <- sampling::UPsystematic(base_sondage$proba_inegal_d1)
  
  return(tirage_bulletins(base_sondage, indic_d1, tour, "inegal",
                          nb_max_bulletins_tires))
  
}

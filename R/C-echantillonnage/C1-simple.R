tirage_simple <- function(base_sondage,
                          nb_bv_tires,
                          nb_max_bulletins_tires,
                          poids_cales,
                          tour = "T1",
                          strate_var = NULL) {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # tour <- "T1"
  
  obs_ech <- sampling::srswor(N = nrow(base_sondage), n = nb_bv_tires)
  
  ech <- getdata(base_sondage, obs_ech)
  
  vecteur <- ifelse(base_sondage$ID %in% ech$ID, 1, 0)
  
  base_sondage$proba_simple_d1 <- nb_bv_tires / nrow(base_sondage)
  
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = vecteur,
                          tour = tour,
                          methode = "simple",
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales))
  
}

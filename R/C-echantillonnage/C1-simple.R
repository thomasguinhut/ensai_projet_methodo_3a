tirage_simple <- function(bdd_sondage,
                          nb_bv_tires,
                          nb_max_bulletins_tires,
                          poids_cales,
                          strate_var = NULL,
                          type_calage = NULL,
                          tour = "T1") {
  
  # nb_bv_tires <- 600
  # nb_bulletins_tires <- 100
  # tour <- "T1"
  
  obs_ech <- sampling::srswor(N = nrow(bdd_sondage), n = nb_bv_tires)
  
  ech <- getdata(bdd_sondage, obs_ech)
  
  vecteur <- ifelse(bdd_sondage$ID %in% ech$ID, 1, 0)
  
  bdd_sondage$proba_d1 <- nb_bv_tires / nrow(bdd_sondage)
  
  return(tirage_bulletins(bdd_sondage = bdd_sondage, 
                          indic_d1 = vecteur,
                          tour = tour,
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales,
                          strate_var = strate_var,
                          type_calage = type_calage))
  
}

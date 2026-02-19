tirage_bulletins <- function(bdd_sondage, indic_d1, tour,
                             nb_max_bulletins_tires, poids_cales, type_calage,
                             strate_var = NULL){
  
  ech_bv <- getdata(bdd_sondage, indic_d1)

  if (poids_cales) {
    ech_bv$proba_d1_cale <- 1 / calage(
      ech = ech_bv, poids = (1 / ech_bv$proba_d1),
      type_calage = type_calage, strate_var = strate_var
    )
  }

  bds_individus <- creer_base_bulletins(ech_bv, tour)

  nbr_bulletins_tires <- ifelse(
    ech_bv[[paste0("EXPRIMES_", tour)]] < nb_max_bulletins_tires,
    ech_bv[[paste0("EXPRIMES_", tour)]], nb_max_bulletins_tires)

  indic_d2 <- sampling::strata(data = bds_individus,
                               stratanames = "ID",
                               size = nbr_bulletins_tires,
                               method = "srswor")

  ech_bulletins <- getdata(bds_individus, indic_d2)

  names(ech_bulletins)[names(ech_bulletins) == "Prob"] <- "proba_d2"
  
  if (poids_cales) {
    
    ech_bulletins <- merge(ech_bulletins,
                           ech_bv[, c("ID", "proba_d1_cale")],
                           by = "ID",
                           all.x = T,
                           all.y = F)
    
    ech_bulletins$poids <-
      1 / (ech_bulletins$proba_d1_cale * ech_bulletins$proba_d2)
    
  } else {
    
    ech_bulletins <- merge(ech_bulletins,
                           ech_bv[, c("ID", "proba_d1")],
                           by = "ID",
                           all.x = T,
                           all.y = F)
    
    ech_bulletins$poids <-
      1 / (ech_bulletins$proba_d1 * ech_bulletins$proba_d2)
    
  }

  return(ech_bulletins)

}

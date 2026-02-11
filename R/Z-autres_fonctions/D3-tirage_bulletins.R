tirage_bulletins <- function(base_sondage, indic_d1, tour, methode,
                             nb_max_bulletins_tires){
  
  ech_bv <- getdata(base_sondage, indic_d1)
  
  bds_individus <- creer_base_bulletins(ech_bv, tour)
  
  nbr_bulletins_tires <- ifelse(
    ech_bv[[paste0("EXPRIMES_", tour)]] < nb_max_bulletins_tires,
    ech_bv[[paste0("EXPRIMES_", tour)]], nb_max_bulletins_tires)
  
  indic_d2 <- sampling::strata(data = bds_individus,
                               stratanames = "ID",
                               size = nbr_bulletins_tires,
                               method = "srswor")
  
  ech_bulletins <- getdata(bds_individus, indic_d2)
  
  names(ech_bulletins)[names(ech_bulletins) == "Prob"] <- paste0("proba_",
                                                                 methode,
                                                                 "_d2")
  
  ech_bulletins <- merge(ech_bulletins,
                         base_sondage[, c("ID", paste0("proba_",
                                                       methode, "_d1"))],
                         by = "ID",
                         all.x = T,
                         all.y = F)
  
  ech_bulletins[[paste0("poids_", methode)]] <-
    1 / (ech_bulletins[[paste0("proba_", methode, "_d1")]] * 
           ech_bulletins[[paste0("proba_", methode, "_d2")]])

  return(ech_bulletins)

}

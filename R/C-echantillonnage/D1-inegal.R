objets_initiaux <- ls()

source("R/C-echantillonnage/D0-creer_base_bulletins.R")

tirage_inegal <- function(base, nb_bv_tires, nb_bulletins_tires, autopondere, tour) {
  
  # base <- bv_2022_final
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # autopondere <- TRUE
  # tour <- "T1"
  
  base <- base %>% 
    filter(TIRABLE)
  
  variable_nbr_inscrits <- paste0("INSCRITS_", tour)
  variable_nbr_exprimes <- paste0("EXPRIMES_", tour)
  
  if(autopondere) {
    
    base <- base %>% 
      filter(base[[variable_nbr_exprimes]] >= nb_bulletins_tires)
    
  }
  
  base$proba_inegal_d1 <- 
    sampling::inclusionprobabilities(
      base[[variable_nbr_inscrits]],
      n = nb_bv_tires)
  
  indic_d1 <- sampling::UPsystematic(base$proba_inegal_d1)
  
  ech_bv <- getdata(base, indic_d1)
  
  bds_individus <- creer_base_bulletins(ech_bv, tour = "T1")
  
  nbr_bulletins_tires <- ifelse(ech_bv[[variable_nbr_exprimes]] < nb_bulletins_tires,
                                ech_bv[[variable_nbr_exprimes]],
                                nb_bulletins_tires)
  
  indic_2d <- sampling::strata(data = bds_individus,
                               stratanames = "ID",
                               size = nbr_bulletins_tires,
                               method = "srswor")
  
  ech_bulletins <- getdata(bds_individus, indic_2d)
  
  names(ech_bulletins)[names(ech_bulletins) == "Prob"] <- "proba_inegal_d2"
  
  ech_bulletins <- merge(ech_bulletins,
                         base[, c("ID", "proba_inegal_d1")],
                             by = "ID",
                             all.x = T,
                             all.y = F)
  
  ech_bulletins$poids <-
    with(ech_bulletins, 1 / (proba_inegal_d1 * proba_inegal_d2))
  
  return(ech_bulletins)
  
}

ech_inegal <- tirage_inegal(bv_2022_final, 500, 100, FALSE, "T1")

hist(ech_inegal$poids)

design_inegal <- svydesign(data = ech_inegal,
                           id = ~ID,
                           fpc = ~proba_inegal_d1,
                           weights = ~poids)

svymean(design = design_inegal, ~I(MACRON*100))
round(sum(bv_2022_final$MACRON_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

svymean(design = design_inegal, ~I(LEPEN*100))
round(sum(bv_2022_final$LEPEN_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

svymean(design = design_inegal, ~I(MELENCHON*100))
round(sum(bv_2022_final$MELENCHON_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

objets_initiaux <- ls()

tirage_inegal <- function(base_sondage, nb_bv_tires, nb_maxe_bulletins_tires,
                          autopondere, tour) {
  
  # base <- bv_2022_final
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # autopondere <- TRUE
  # tour <- "T1"
  
  if(autopondere) {
    base_sondage <- base_sondage %>% 
      filter(
        base_sondage[[paste0("EXPRIMES_", tour)]] >= paste0("EXPRIMES_", tour)
      )
  }
  
  base_sondage$proba_inegal_d1 <- 
    sampling::inclusionprobabilities(
      base_sondage[[paste0("INSCRITS_", tour)]],
      n = nb_bv_tires)
  
  indic_d1 <- sampling::UPsystematic(base_sondage$proba_inegal_d1)
  
  return(tirage_bulletins(base_sondage, indic_d1, tour,
                          ifelse(autopondere, "inegal_autopond", "inegal"),
                          nb_maxe_bulletins_tires))
  
}

ech_inegal <- tirage_inegal(bv_2022_final, 500, 100, FALSE, "T1")

design_inegal <- svydesign(data = ech_inegal,
                           id = ~ID,
                           fpc = ~proba_inegal_d1,
                           weights = ~poids_inegal)

svymean(design = design_inegal, ~I(MACRON*100))
round(sum(bv_2022_final$MACRON_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

svymean(design = design_inegal, ~I(LEPEN*100))
round(sum(bv_2022_final$LEPEN_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

svymean(design = design_inegal, ~I(MELENCHON*100))
round(sum(bv_2022_final$MELENCHON_T1) / sum(bv_2022_final$EXPRIMES_T1) * 100, 1)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

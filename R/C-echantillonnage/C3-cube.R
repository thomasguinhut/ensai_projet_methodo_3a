tirage_cube <- function(nb_bv_tires, nb_max_bulletins_tires,
                        stratifie = FALSE, tour = "T1") {

  x <- base_sondage %>% 
    dplyr::select(
      "ID",
      ends_with("2017_T1"),
      "DENS3",
      starts_with(c("IND", "MEN", "LOG")), 
      -c("IND", "MEN", "LOG"),
      -starts_with("PROP")
    ) %>% 
    mutate(
      RURAL = ifelse(DENS3 == "3", 1, 0),
      URBAIN_INTERM = ifelse(DENS3 == "2", 1, 0),
      URBAIN_DENSE = ifelse(DENS3 == "1", 1, 0)
    ) %>% 
    dplyr::select(ID, INSCRITS_2017_T1, EXPRIMES_2017_T1, MACRON_2017_T1,
                  LEPEN_2017_T1, FILLON_2017_T1, MELENCHON_2017_T1, HAMON_2017_T1,
                  DUPONTAIGNAN_2017_T1, LASSALLE_2017_T1, POUTOU_2017_T1,
                  ASSELINEAU_2017_T1, ARTHAUD_2017_T1, CHEMINADE_2017_T1,
                  RURAL, URBAIN_INTERM, URBAIN_DENSE, starts_with("MEN"), "LOG_SOC",
                  rev(starts_with("IND")), -DENS3, -VOTANTS_2017_T1) %>% 
    as.data.frame()
  
  row.names(x) <- x$ID
  x$ID <- NULL
  
  n <- nb_bv_tires
  N <- nrow(base_sondage)
  PI <- rep(n/N, N)
  
  bdd_cube <- cbind(PI, as.matrix(x))
  
  base_sondage$proba_cube_d1 <- PI
  
  ech <- samplecube(bdd_cube, PI, method = 2, comment = FALSE)
  
  return(tirage_bulletins(base_sondage, ech, "T1", "cube",
                          nb_max_bulletins_tires))

}



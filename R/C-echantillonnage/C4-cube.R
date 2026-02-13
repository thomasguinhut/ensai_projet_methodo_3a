tirage_cube <- function(base_sondage,
                        nb_bv_tires,
                        nb_max_bulletins_tires,
                        type_strat,
                        poids_cales,
                        stratifie = FALSE,
                        tour = "T1",
                        strate_var = NULL,
                        comment_cube = FALSE) {

  # nb_bv_tires <- 600
  # nb_bulletins_tires <- 100
  # poids_cales <- TRUE
  # stratifie <- TRUE
  # tour <- "T1"
  # strate_var <- "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8"
  # comment_cube <- TRUE
  
  bdd_cube <- as.data.frame(base_sondage)
  
  x <- bdd_cube %>% 
    arrange(.data[[strate_var]]) %>% 
    dplyr::select(
      "ID", "TIRABLE",
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
    dplyr::select(ID, TIRABLE, INSCRITS_2017_T1, EXPRIMES_2017_T1, MACRON_2017_T1,
                  LEPEN_2017_T1, FILLON_2017_T1, MELENCHON_2017_T1, HAMON_2017_T1,
                  DUPONTAIGNAN_2017_T1, LASSALLE_2017_T1, POUTOU_2017_T1,
                  ASSELINEAU_2017_T1, ARTHAUD_2017_T1, CHEMINADE_2017_T1,
                  RURAL, URBAIN_INTERM, URBAIN_DENSE, starts_with("MEN"), "LOG_SOC",
                  rev(starts_with("IND")), -DENS3, -VOTANTS_2017_T1) %>% 
    as.data.frame()
  
  row.names(x) <- x$ID
  x$ID <- NULL
  
  PI <- calcul_nh(base_sondage = base_sondage,
                  nb_bv_tires = nb_bv_tires,
                  return_pik = TRUE,
                  type_strat = type_strat,
                  strate_var = strate_var)
  
  X <- cbind(PI, as.matrix(x))
  
  if (stratifie) {
    base_sondage$proba_cubestrat_d1 <- PI
  } else {
    base_sondage$proba_cube_d1 <- PI
  }
  
  if (stratifie) {
    ech <- balancedstratification(X,
                                  bdd_cube[[strate_var]],
                                  PI, comment=comment_cube, method=2)
  } else {
    ech <- samplecube(X, PI, method = 2, comment = comment_cube)
  }
  
  nom_methode <- if (stratifie) "cubestrat" else "cube"
  
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = ech,
                          tour = tour,
                          methode = nom_methode,
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales,
                          strate_var = strate_var))
}
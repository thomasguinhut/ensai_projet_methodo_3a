tirage_cube <- function(nb_bv_tires, nb_max_bulletins_tires,
                        stratifie = FALSE, calage = FALSE, tour = "T1") {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # stratifie <- FALSE
  # calage <- FALSE
  # tour <- "T1"
  
  bdd_cube <- as.data.frame(if (calage & !calage) {
    bv_2022_final
  } else if (calage & stratifie) {
    bv_2022_final %>% filter(!is.na(CLUSTER_AFM_DENSITE_FILOSOFI))
  } else {base_sondage})

  x <- bdd_cube %>% 
    dplyr::select(
      "ID", "TIRABLE",
      ends_with("2017_T1"),
      "DENS3",
      starts_with(c("IND", "MEN", "LOG")), 
      CLUSTER_AFM_DENSITE_FILOSOFI_2017,
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
  
  if (calage) x <- x %>% arrange(desc(TIRABLE))
  
  n <- nb_bv_tires
  N <- nrow(base_sondage)
  PI <- if (calage) c(rep(n/N, N),
                      rep(0, nrow(bdd_cube) - N)) else rep(n/N, N)
  
  X <- cbind(PI, as.matrix(x))
  
  if (calage & !stratifie) {
    base_sondage$proba_cubecale_d1 <- rep(n/N, N)
  } else if (stratifie & !calage) {
    base_sondage$proba_cubestrat_d1 <- rep(n/N, N)
  } else if (stratifie & calage) {
    base_sondage$proba_cubestratcale_d1 <- rep(n/N, N)
  } else {
    base_sondage$proba_cube_d1 <- rep(n/N, N)
  }
  
  if (stratifie) {
    ech <- balancedstratification(X,
                                  bdd_cube$CLUSTER_AFM_DENSITE_FILOSOFI_2017,
                                  PI, comment=FALSE, method=2)
  } else {
    ech <- samplecube(X, PI, method = 2, comment = FALSE)
  }
  
  if (calage) ech <- ech[1:N]
  
  if (calage & !stratifie) {
    nom_methode <- "cubecale"
  } else if (!calage & stratifie) {
    nom_methode <- "cubestrat"
  } else if (calage & stratifie) {
    nom_methode <- "cubestratcale"
  } else {
    nom_methode <- "cube"
  }
  
  return(tirage_bulletins(base_sondage, ech, "T1", nom_methode,
                          nb_max_bulletins_tires))

}



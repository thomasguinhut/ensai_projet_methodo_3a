tirage_stratifie <- function(bdd_sondage,
                             nb_bv_tires,
                             nb_max_bulletins_tires,
                             type_strat,
                             prez2017,
                             filosofi,
                             nb_clusters,
                             poids_cales,
                             type_calage,
                             tour = "T1") {
  
  # bdd_sondage <- base_sondage
  # nb_bv_tires <- 500
  # nb_max_bulletins_tires <- 100
  # type_strat <- NULL
  # prez2017 <- TRUE
  # filosofi <- TRUE
  # nb_clusters <- 8
  # poids_cales <- TRUE
  # tour <- "T1"
  
  strate_var <- if (prez2017 & filosofi) {
    paste0("CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_", nb_clusters)
  } else if (!prez2017 & filosofi) {
    paste0("CLUSTER_AFM_IDF_DENSITE_FILOSOFI_", nb_clusters)
  } else if (prez2017 & !filosofi) {
    paste0("CLUSTER_AFM_IDF_DENSITE_2017_", nb_clusters)
  }
  
  bdd_sondage_inter <- bdd_sondage[order(bdd_sondage[[strate_var]]), ]
  
  Nh <- calcul_nh(bdd_sondage = bdd_sondage_inter,
                  nb_bv_tires = nb_bv_tires,
                  return_pik = FALSE,
                  type_strat = type_strat,
                  strate_var = strate_var)
  
  # Tirage stratifié
  obs_ech <- sampling::strata(
    data = bdd_sondage_inter,
    stratanames = strate_var,
    size = Nh$alloc,
    method = "srswor"
  )
  
  # Extraction de l'échantillon
  ech <- getdata(bdd_sondage_inter, obs_ech)
  
  # Vecteur indicateur
  vecteur <- ifelse(bdd_sondage$ID %in% ech$ID, 1, 0)
  
  bdd_sondage <- bdd_sondage %>%
    left_join(Nh, by = strate_var)

  bdd_sondage$proba_d1 <- bdd_sondage$alloc / bdd_sondage$Nh

  bdd_sondage <- bdd_sondage %>% dplyr::select(-Nh, -alloc)
  
  # Tirage des bulletins
  return(tirage_bulletins(bdd_sondage = bdd_sondage, 
                          indic_d1 = vecteur,
                          tour = tour,
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales,
                          strate_var = strate_var,
                          type_calage = type_calage))

}

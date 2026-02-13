tirage_stratifie <- function(base_sondage,
                             nb_bv_tires,
                             nb_max_bulletins_tires,
                             type_strat,
                             annee2017,
                             filosofi,
                             nb_clusters,
                             poids_cales,
                             tour = "T1") {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # annee2017 <- TRUE
  # nb_clusters <- 5
  # poids_cales <- TRUE
  # tour <- "T1"
  
  strate_var <- if (annee2017 & filosofi) {
    paste0("CLUSTER_AFM_DENSITE_FILOSOFI_2017_", nb_clusters)
  } else if (!annee2017 & filosofi) {
    paste0("CLUSTER_AFM_DENSITE_FILOSOFI_", nb_clusters)
  } else if (annee2017 & !filosofi) {
    paste0("CLUSTER_AFM_DENSITE_2017_", nb_clusters)
  }
  
  methode <- if (annee2017 & filosofi) {
    "stratfilosofi2017"
  } else if (!annee2017 & filosofi) {
    "stratfilosofi"
  } else if (annee2017 & !filosofi) {
    "strat2017"
  }
  
  proba_var <- if (annee2017 & filosofi) {
    "proba_stratfilosofi2017_d1"
  } else if (!annee2017 & filosofi) {
    "proba_stratfilosofi_d1"
  } else if (annee2017 & !filosofi) {
    "proba_strat2017_d1"
  }
  
  base_sondage_inter <- base_sondage[order(base_sondage[[strate_var]]), ]
  
  # Tirage stratifié
  obs_ech <- sampling::strata(
    data = base_sondage_inter,
    stratanames = strate_var,
    size = calcul_nh(base_sondage = base_sondage,
                     nb_bv_tires = nb_bv_tires,
                     return_pik = FALSE,
                     type_strat = type_strat,
                     strate_var = strate_var)$alloc,
    method = "srswor"
  )
  
  # Extraction de l'échantillon
  ech <- getdata(base_sondage_inter, obs_ech)
  
  # Vecteur indicateur
  vecteur <- ifelse(base_sondage$ID %in% ech$ID, 1, 0)
  
  base_sondage <- base_sondage %>%
    left_join(Nh, by = strate_var)

  base_sondage[[proba_var]] <-
    base_sondage$alloc / base_sondage$Nh

  base_sondage <- base_sondage %>% dplyr::select(-Nh, -alloc)
  
  # Tirage des bulletins
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = vecteur,
                          tour = tour,
                          methode = methode,
                          nb_max_bulletins_tires = nb_max_bulletins_tires,
                          poids_cales = poids_cales,
                          strate_var = strate_var))

}

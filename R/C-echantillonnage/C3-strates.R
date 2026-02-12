tirage_stratifie <- function(nb_bv_tires,
                             nb_max_bulletins_tires,
                             annee2017,
                             nb_clusters,
                             poids_cales,
                             alloc_opt = FALSE,
                             tour = "T1") {
  
  # nb_bv_tires <- 500
  # nb_bulletins_tires <- 100
  # annee2017 <- TRUE
  # nb_clusters <- 5
  # poids_cales <- TRUE
  # alloc_opt <- FALSE
  # tour <- "T1"
  
  strate_var <- if (annee2017) {
    paste0("CLUSTER_AFM_DENSITE_FILOSOFI_2017_", nb_clusters)
  } else {
    paste0("CLUSTER_AFM_DENSITE_FILOSOFI_", nb_clusters)
  }
  
  methode <- if (annee2017) {
    "stratfilosofi2017"
  } else {
    "stratfilosofi"
  }
  
  somme_var <- if (annee2017) {
    "somme_EXPRIMES_T1_2017"
  } else {
    "somme_EXPRIMES_T1"
  }
  
  proba_var <- if (annee2017) {
    "proba_stratfilosofi2017_d1"
  } else {
    "proba_stratfilosofi_d1"
  }
  
  # Calcul du nombre d'observations par strate
  Nh <- base_sondage %>%
    group_by(.data[[strate_var]]) %>%
    summarise(Nh = n(), .groups = "drop")
    
  Nh_vect <- Nh %>% pull(Nh)
  
  S_yh <- aggregate(reformulate(strate_var, "LEPEN_2017_T1"),
                    data = base_sondage, 
                    FUN = var,
                    na.rm = TRUE)$LEPEN_2017_T1
  
  # Allocation optimales
  den <- t(Nh_vect) %*% S_yh
  n_opt <- round(c(1000/den)*(Nh_vect*S_yh))
  
  # Allocation proportionnelle sinon
  if (alloc_opt) {
    Nh$alloc <- n_opt
  } else {
    Nh$alloc <- round(Nh$Nh * nb_bv_tires / nrow(base_sondage))
  }
  
  # S'assurer qu'il y a au moins 1 par strate
  Nh$alloc <- pmax(Nh$alloc, 1)
  
  # Ajustement pour respecter nb_bv_tires
  difference <- nb_bv_tires - sum(Nh$alloc)
  if (difference != 0) {
    Nh$alloc[which.max(Nh$alloc)] <- Nh$alloc[which.max(Nh$alloc)] + difference
  }
  
  # Tri
  Nh <- as.data.frame(Nh) %>% 
    arrange(.data[[strate_var]])
  base_sondage_inter <- base_sondage[order(base_sondage[[strate_var]]), ]
  
  # Tirage stratifié
  obs_ech <- sampling::strata(
    data = base_sondage_inter,
    stratanames = strate_var,
    size = Nh$alloc,
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

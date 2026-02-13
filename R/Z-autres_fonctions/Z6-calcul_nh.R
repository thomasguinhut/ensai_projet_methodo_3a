calcul_nh <- function(base_sondage,
                      nb_bv_tires,
                      return_pik,
                      strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
                      type_strat = NULL) {
  
  # nb_bv_tires <- 600
  # return_pik <- TRUE
  # strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8"
  # type_strat = NULL
  
  if (!is.null(type_strat) && type_strat == "idf") {
    # Séparer les strates HDF et IDF
    base_hdf <- base_sondage %>% 
      filter(grepl("^hdf_", .data[[strate_var]]))
    
    base_idf <- base_sondage %>% 
      filter(grepl("^idf_", .data[[strate_var]]))
    
    # Répartir nb_bv_tires entre HDF et IDF
    nb_bv_hdf <- floor(nb_bv_tires / 2)
    nb_bv_idf <- nb_bv_tires - nb_bv_hdf  # Pour gérer les nombres impairs
    
    # Fonction auxiliaire pour calculer l'allocation
    calc_alloc <- function(base, nb_bv) {
      # Calcul du nombre d'observations par strate
      Nh <- base %>%
        group_by(.data[[strate_var]]) %>%
        summarise(Nh = n(), .groups = "drop")
      
      # Allocation proportionnelle
      Nh$alloc <- round(Nh$Nh * nb_bv / nrow(base))
      
      # S'assurer qu'il y a au moins 1 par strate
      Nh$alloc <- pmax(Nh$alloc, 1)
      
      # Ajustement pour respecter nb_bv
      difference <- nb_bv - sum(Nh$alloc)
      if (difference != 0) {
        Nh$alloc[which.max(Nh$alloc)] <- Nh$alloc[which.max(Nh$alloc)] + difference
      }
      
      return(Nh)
    }
    
    # Calculer les allocations pour HDF et IDF séparément
    Nh_hdf <- calc_alloc(base_hdf, nb_bv_hdf)
    Nh_idf <- calc_alloc(base_idf, nb_bv_idf)
    
    # Combiner les résultats
    Nh <- bind_rows(Nh_hdf, Nh_idf) %>%
      as.data.frame() %>%
      arrange(.data[[strate_var]])
    
  } else if (!is.null(type_strat) && type_strat == "egal") {
    # Allocation égale dans chaque strate
    Nh <- base_sondage %>%
      group_by(.data[[strate_var]]) %>%
      summarise(Nh = n(), .groups = "drop")
    
    # Nombre de strates
    nb_strates <- nrow(Nh)
    
    # Allocation égale de base
    alloc_base <- floor(nb_bv_tires / nb_strates)
    
    # Plafonner par Nh et calculer les allocations
    Nh$alloc <- pmin(alloc_base, Nh$Nh)
    
    # Calculer le surplus à redistribuer
    surplus <- nb_bv_tires - sum(Nh$alloc)
    
    # Redistribuer le surplus sur les strates qui peuvent encore en recevoir
    while (surplus > 0) {
      # Trouver les strates qui ne sont pas plafonnées
      strates_dispo <- which(Nh$alloc < Nh$Nh)
      
      if (length(strates_dispo) == 0) {
        # Toutes les strates sont plafonnées, impossible d'allouer plus
        warning(paste("Impossible d'allouer", nb_bv_tires, "BV. Maximum possible:", sum(Nh$alloc)))
        break
      }
      
      # Répartir le surplus équitablement sur les strates disponibles
      alloc_supp <- floor(surplus / length(strates_dispo))
      if (alloc_supp == 0) alloc_supp <- 1
      
      for (i in strates_dispo) {
        if (surplus == 0) break
        ajout <- min(alloc_supp, Nh$Nh[i] - Nh$alloc[i], surplus)
        Nh$alloc[i] <- Nh$alloc[i] + ajout
        surplus <- surplus - ajout
      }
    }
    
    # Tri
    Nh <- as.data.frame(Nh) %>% 
      arrange(.data[[strate_var]])
    
  } else {
    # Calcul classique sans stratification particulière
    Nh <- base_sondage %>%
      group_by(.data[[strate_var]]) %>%
      summarise(Nh = n(), .groups = "drop")
    
    # Allocation proportionnelle
    Nh$alloc <- round(Nh$Nh * nb_bv_tires / nrow(base_sondage))
    
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
  }
  
  # Retourner les probabilités d'inclusion si demandé
  if (return_pik) {
    print(Nh)
    # Joindre les allocations à la base de sondage
    base_avec_alloc <- base_sondage %>%
      left_join(Nh %>% 
                  dplyr::select(.data[[strate_var]], Nh, alloc),
                by = strate_var) %>% 
      arrange(.data[[strate_var]])
    
    # Calculer les probabilités d'inclusion : pik = nh / Nh
    pik <- base_avec_alloc$alloc / base_avec_alloc$Nh
    
    return(pik)
  } else {
    return(Nh)
  }
}

# print(calcul_nh(base_sondage = base_sondage,
#                 nb_bv_tires = 600,
#                 return_pik = FALSE,
#                 strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
#                 type_strat = NULL))
# print(calcul_nh(base_sondage = base_sondage,
#                 nb_bv_tires = 600,
#                 return_pik = TRUE,
#                 strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
#                 type_strat = NULL))
# 
# print(calcul_nh(base_sondage = base_sondage,
#                 nb_bv_tires = 600,
#                 return_pik = FALSE,
#                 strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
#                 type_strat = "idf"))
# 
# print(calcul_nh(base_sondage = base_sondage,
#                 nb_bv_tires = 600,
#                 return_pik = FALSE,
#                 strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
#                 type_strat = "egal"))

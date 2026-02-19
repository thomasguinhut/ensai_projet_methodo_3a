calage <- function(ech, poids, type_calage = "poststrat", strate_var = NULL,
                   method_calage = "linear") {
  
  # strate_var <- "CLUSTER_AFM_DENSITE_FILOSOFI_6"
  # ech_bv <- ech_simple
  # type_calage <- "marge"
  # poids <- 1 / ech_bv$proba_d1

  if (type_calage == "poststrat") {
  
    filter_expr <- if (grepl("2017", strate_var)) {
      bv_2022_final %>% filter(!is.na(.data[[strate_var]]))
    } else {
      bv_2022_final
    }
    
    valeurs_cluster <- filter_expr %>%
      pull(.data[[strate_var]]) %>%
      unique() %>%
      sort()
    
    # Créer dynamiquement les variables cluster
    base_totaux <- bv_2022_final %>% 
      filter(!is.na(.data[[strate_var]]))
    
    # Ajouter les colonnes cluster de manière dynamique
    for (val in valeurs_cluster) {
      col_name <- paste0("cluster_", val)
      base_totaux <- base_totaux %>% 
        mutate(!!col_name := ifelse(.data[[strate_var]] == as.character(val), 1, 0))
    }
    
    # Sélectionner uniquement les colonnes cluster
    cluster_cols <- paste0("cluster_", valeurs_cluster)
    base_totaux <- base_totaux %>% 
      dplyr::select(all_of(cluster_cols))
    
    # Calculer le nombre de bureaux par cluster
    totaux <- as.vector(colSums(base_totaux))
    
    # Créer la matrice X pour l'échantillon
    X <- bv_2022_final %>% 
      filter(ID %in% ech$ID)
    
    # Ajouter les colonnes cluster de manière dynamique
    for (val in valeurs_cluster) {
      col_name <- paste0("cluster_", val)
      X <- X %>% 
        mutate(!!col_name := ifelse(.data[[strate_var]] == as.character(val), 1, 0))
    }
    
    X <- X %>% 
      dplyr::select(all_of(cluster_cols)) %>% 
      as.matrix()
    
  } else {
    
    bdd_calage <- as.data.frame(bv_2022_final)
    
    X <- bdd_calage %>% 
      dplyr::select(
        "ID",
        ends_with(c("2017_T1")),
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
                    LEPEN_2017_T1, FILLON_2017_T1,
                    MELENCHON_2017_T1, HAMON_2017_T1,
                    DUPONTAIGNAN_2017_T1, LASSALLE_2017_T1, POUTOU_2017_T1,
                    ASSELINEAU_2017_T1, ARTHAUD_2017_T1, CHEMINADE_2017_T1,
                    RURAL, URBAIN_INTERM, URBAIN_DENSE, starts_with("MEN"), "LOG_SOC",
                    rev(starts_with("IND")), -DENS3, -VOTANTS_2017_T1) %>% 
      as.data.frame()
    
    row.names(X) <- X$ID
    X$ID <- NULL
    
    marges <- colnames(X)
    
    totaux <- as.vector(colSums(X[,marges]))
    
    X <- X[ech$ID,]
    
  }
  
  # Calibration
  g_poids_lineaire <- calib(X, poids, totaux, method = method_calage)
  
  return(g_poids_lineaire * poids)
}
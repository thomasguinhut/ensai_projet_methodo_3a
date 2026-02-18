calage <- function(ech, poids, type_calage = "poststrat", strate_var = NULL,
                   marges) {
  
  strate_var <- "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_8"
  
  # Identifier les valeurs uniques du cluster
  valeurs_cluster <- bv_2022_final %>% 
    filter(!is.na(.data[[strate_var]])) %>% 
    pull(.data[[strate_var]]) %>% 
    unique() %>% 
    sort()
  
  # Créer dynamiquement les variables cluster
  base_totaux <- bv_2022_final %>% 
    filter(!is.na(.data[[strate_var]]))
  
  # Ajouter les colonnes cluster de manière dynamique
  for (val in valeurs_cluster) {
    col_name <- paste0("cluster", val)
    base_totaux <- base_totaux %>% 
      mutate(!!col_name := ifelse(.data[[strate_var]] == as.character(val), 1, 0))
  }
  
  # Sélectionner EXPRIMES_T1 et toutes les colonnes cluster
  cluster_cols <- paste0("cluster", valeurs_cluster)
  base_totaux <- base_totaux %>% 
    dplyr::select(EXPRIMES_T1, all_of(cluster_cols))
  
  # Calculer les totaux
  totaux <- as.vector(colSums(sweep(base_totaux[, -1], 1, base_totaux$EXPRIMES_T1, `*`)))
  
  # Créer la matrice X pour l'échantillon
  X <- bv_2022_final %>% 
    right_join(ech, by = "ID") %>% 
    filter(!is.na(.data[[strate_var]]))
  
  # Ajouter les colonnes cluster de manière dynamique
  for (val in valeurs_cluster) {
    col_name <- paste0("cluster", val)
    X <- X %>% 
      mutate(!!col_name := ifelse(.data[[strate_var]] == as.character(val), 1, 0))
  }
  
  X <- X %>% 
    dplyr::select(all_of(cluster_cols)) %>% 
    as.matrix()
  
  # Calibration
  g_poids_lineaire <- calib(X, poids, totaux, method = "linear")
  
  return(g_poids_lineaire * poids)
}
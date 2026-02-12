calage <- function(ech, poids, strate_var = NULL) {
  
  # Si strate_var n'est pas fourni, utiliser la variable par défaut
  if (is.null(strate_var)) {
    strate_var <- "CLUSTER_AFM_DENSITE_FILOSOFI_2017"
  }
  
  base_totaux <- bv_2022_final %>% 
    filter(!is.na(.data[[strate_var]])) %>% 
    mutate(cluster1 = ifelse(.data[[strate_var]] == "1", 1, 0),
           cluster2 = ifelse(.data[[strate_var]] == "2", 1, 0),
           cluster3 = ifelse(.data[[strate_var]] == "3", 1, 0),
           cluster4 = ifelse(.data[[strate_var]] == "4", 1, 0),
           cluster5 = ifelse(.data[[strate_var]] == "5", 1, 0)) %>% 
    dplyr::select(EXPRIMES_T1, cluster1, cluster2, cluster3, cluster4, cluster5)
  
  totaux <- as.vector(colSums(sweep(base_totaux[, -1], 1, base_totaux$EXPRIMES_T1, `*`)))
  
  X <- bv_2022_final %>% 
    right_join(ech, by = "ID") %>% 
    filter(!is.na(.data[[strate_var]])) %>% 
    mutate(cluster1 = ifelse(.data[[strate_var]] == "1", 1, 0),
           cluster2 = ifelse(.data[[strate_var]] == "2", 1, 0),
           cluster3 = ifelse(.data[[strate_var]] == "3", 1, 0),
           cluster4 = ifelse(.data[[strate_var]] == "4", 1, 0),
           cluster5 = ifelse(.data[[strate_var]] == "5", 1, 0)) %>% 
    dplyr::select(cluster1, cluster2, cluster3, cluster4, cluster5) %>% 
    as.matrix()
  
  g_poids_lineaire <- calib(X, poids, totaux, method = "linear")
  
  return(g_poids_lineaire * poids)
  
}

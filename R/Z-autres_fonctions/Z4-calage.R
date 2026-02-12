calage <- function(ech, poids) {
  
  base_totaux <- bv_2022_final %>% 
    filter(!is.na(CLUSTER_AFM_DENSITE_FILOSOFI_2017)) %>% 
    mutate(cluster1 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "1", 1, 0),
           cluster2 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "2", 1, 0),
           cluster3 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "3", 1, 0),
           cluster4 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "4", 1, 0),
           cluster5 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "5", 1, 0)) %>% 
    dplyr::select(EXPRIMES_T1, cluster1, cluster2, cluster3, cluster4, cluster5)
  
  totaux <- as.vector(colSums(sweep(base_totaux[, -1], 1, base_totaux$EXPRIMES_T1, `*`)))
  
  X <- bv_2022_final %>% 
    right_join(ech, by = "ID") %>% 
    filter(!is.na(CLUSTER_AFM_DENSITE_FILOSOFI_2017)) %>% 
    mutate(cluster1 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "1", 1, 0),
           cluster2 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "2", 1, 0),
           cluster3 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "3", 1, 0),
           cluster4 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "4", 1, 0),
           cluster5 = ifelse(CLUSTER_AFM_DENSITE_FILOSOFI_2017 == "5", 1, 0)) %>% 
    dplyr::select(cluster1, cluster2, cluster3, cluster4, cluster5) %>% 
    as.matrix()
  
  g_poids_lineaire <- calib(X, poids, totaux, method="linear")
  
  return(g_poids_lineaire * poids)
  
}
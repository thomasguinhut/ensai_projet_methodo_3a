tirage_stratifie <- function(nb_bv_tires, nb_max_bulletins_tires, tour = "T1") {
  
  # Calculer le nombre d'observations par strate
  Nh <- base_sondage %>%
    group_by(CLUSTER_AFM_DENSITE_FILOSOFI) %>%
    summarise(Nh = n())
  
  # Allouer proportionnellement le nombre d'observations à tirer par strate
  Nh$alloc <- round(Nh$Nh * nb_bv_tires / nrow(base_sondage))
  
  # Ajustement pour que la somme des allocations soit égale à n
  difference <- nb_bv_tires - sum(Nh$alloc)
  if (difference != 0) {
    Nh$alloc[which.max(Nh$alloc)] <- Nh$alloc[which.max(Nh$alloc)] + difference
  }
  
  # Trier les données et les strates
  Nh <- Nh[order(Nh$CLUSTER_AFM_DENSITE_FILOSOFI), ]
  base_sondage_inter <- base_sondage[order(base_sondage$CLUSTER_AFM_DENSITE_FILOSOFI), ]
  
  # Tirer l'échantillon stratifié
  obs_ech <- sampling::strata(
    data = base_sondage_inter,
    stratanames = 'CLUSTER_AFM_DENSITE_FILOSOFI',
    size = Nh$alloc,
    method = 'srswor'
  )
  
  # Extraire les données de l'échantillon
  ech <- getdata(base_sondage_inter, obs_ech)
  
  # Créer le vecteur indicateur
  vecteur <- ifelse(base_sondage$ID %in% ech$ID, 1, 0)
  
  # Calculer la probabilité stratifiée
  somme_par_groupe <- base_sondage %>%
    group_by(CLUSTER_AFM_DENSITE_FILOSOFI) %>%
    summarise(somme_EXPRIMES_T1 = sum(EXPRIMES_T1)) %>%
    ungroup()
  
  base_sondage <- base_sondage %>%
    left_join(somme_par_groupe, by = "CLUSTER_AFM_DENSITE_FILOSOFI")
  
  base_sondage$proba_stratfilosofi_d1 <- base_sondage$EXPRIMES_T1 / base_sondage$somme_EXPRIMES_T1
  
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = vecteur,
                          tour = "T1",
                          methode = "stratfilosofi",
                          nb_max_bulletins_tires = nb_max_bulletins_tires))

}



tirage_stratifie_vote_prec <- function(nb_bv_tires, nb_max_bulletins_tires, tour = "T1") {
  
  # Calculer le nombre d'observations par strate
  Nh <- base_sondage %>%
    group_by(CLUSTER_AFM_DENSITE_FILOSOFI_2017) %>%
    summarise(Nh = n())
  
  # Allouer proportionnellement le nombre d'observations à tirer par strate
  Nh$alloc <- round(Nh$Nh * nb_bv_tires / nrow(base_sondage))
  
  # Ajustement pour que la somme des allocations soit égale à n
  difference <- nb_bv_tires - sum(Nh$alloc)
  if (difference != 0) {
    Nh$alloc[which.max(Nh$alloc)] <- Nh$alloc[which.max(Nh$alloc)] + difference
  }
  
  # Trier les données et les strates
  Nh <- Nh[order(Nh$CLUSTER_AFM_DENSITE_FILOSOFI_2017), ]
  base_sondage_inter <- base_sondage[order(base_sondage$CLUSTER_AFM_DENSITE_FILOSOFI_2017), ]
  
  # Tirer l'échantillon stratifié
  obs_ech <- sampling::strata(
    data = base_sondage_inter,
    stratanames = 'CLUSTER_AFM_DENSITE_FILOSOFI_2017',
    size = Nh$alloc,
    method = 'srswor'
  )
  
  # Extraire les données de l'échantillon
  ech <- getdata(base_sondage_inter, obs_ech)
  
  # Créer le vecteur indicateur
  vecteur <- ifelse(base_sondage$ID %in% ech$ID, 1, 0)
  
  # Calculer la probabilité stratifiée
  somme_par_groupe <- base_sondage %>%
    group_by(CLUSTER_AFM_DENSITE_FILOSOFI_2017) %>%
    summarise(somme_EXPRIMES_T1_2017 = sum(EXPRIMES_T1)) %>%
    ungroup()
  
  base_sondage <- base_sondage %>%
    left_join(somme_par_groupe, by = "CLUSTER_AFM_DENSITE_FILOSOFI_2017")
  
  base_sondage$proba_stratfilosofi2017_d1 <- base_sondage$EXPRIMES_T1 / base_sondage$somme_EXPRIMES_T1_2017
  
  return(tirage_bulletins(base_sondage = base_sondage, 
                          indic_d1 = vecteur,
                          tour = "T1",
                          methode = "stratfilosofi2017",
                          nb_max_bulletins_tires = nb_max_bulletins_tires))
  
}




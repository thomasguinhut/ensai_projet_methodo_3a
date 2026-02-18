executer_tous_plans <- function(bdd_sondage,
                                nb_bv_tires,
                                nb_max_bulletins_tires,
                                tour = "T1",
                                simple = TRUE,
                                simple_cale = TRUE,
                                stratfilosofi_cale = TRUE,
                                stratfilosofi2017_cale = TRUE,
                                cube_filosofi2017_cale= TRUE,
                                cubestrat_filosofi2017_cale3 = TRUE,
                                cubestrat_filosofi2017_cale8 = TRUE,
                                candidats = c("MACRON", "LEPEN", "MELENCHON")) {
  
  resultats <- list()
  
  # 1. Tirage simple sans calage
  if (simple) {

    ech_simple <- tirage_simple(bdd_sondage = bdd_sondage,
                                nb_bv_tires = nb_bv_tires,
                                nb_max_bulletins_tires = nb_max_bulletins_tires,
                                poids_cales = FALSE,
                                tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_simple, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "simple",
        estimation = estimation
      )
    }
  }
    
  # 2. Tirage simple avec calage sur 5 clusters filosofi
  if (simple_cale) {

    ech_simple_cale <- tirage_simple(bdd_sondage = bdd_sondage,
                                     nb_bv_tires = nb_bv_tires,
                                     nb_max_bulletins_tires = nb_max_bulletins_tires,
                                     strate_var = "CLUSTER_AFM_REG_DENSITE_FILOSOFI_5",
                                     poids_cales = TRUE,
                                     tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_simple_cale, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "simple_cale",
        estimation = estimation
      )
    }
  }
  
  # 3. Tirage stratifié filosofi avec calage sur 5 clusters filosofi
  if (stratfilosofi_cale) {

    ech_stratfilosofi_cale <- tirage_stratifie(bdd_sondage = bdd_sondage,
                                               nb_bv_tires = nb_bv_tires,
                                               nb_max_bulletins_tires = nb_max_bulletins_tires,
                                               type_strat = "egal",
                                               annee2017 = FALSE,
                                               filosofi = TRUE,
                                               nb_clusters = "5",
                                               poids_cales = TRUE,
                                               tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_stratfilosofi_cale, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "stratfilosofi_cale",
        estimation = estimation
      )
    }
  }
  
  # 4. Tirage stratifié filosofi2017 avec calage sur 5 clusters filosofi x 2017
  if (stratfilosofi2017_cale) {

    ech_stratfilosofi2017_cale <- tirage_stratifie(bdd_sondage = bdd_sondage,
                                                   nb_bv_tires = nb_bv_tires,
                                                   nb_max_bulletins_tires = nb_max_bulletins_tires,
                                                   type_strat = "egal",
                                                   annee2017 = TRUE,
                                                   filosofi = TRUE,
                                                   nb_clusters = "3",
                                                   poids_cales = TRUE,
                                                   tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_stratfilosofi2017_cale, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "stratfilosofi2017_cale",
        estimation = estimation
      )
    }
  }
  
  # 5. Tirage cube avec calage sur 5 clusters filosofi x 2017
  if (cube_filosofi2017_cale) {

    ech_cube_cale <- tirage_cube(bdd_sondage = bdd_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 poids_cales = TRUE,
                                 stratifie = FALSE,
                                 tour = tour,
                                 strate_var = "CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_3",
                                 comment_cube = FALSE)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cube_cale, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cube_filosofi2017_cale",
        estimation = estimation
      )
    }
  }
  
  # 6. Tirage cube stratifié idf avec calage sur 5 clusters filosofi x 2017
  if (cubestrat_filosofi2017_cale3) {
    
    ech_cubestrat_cale5 <- tirage_cube(bdd_sondage = bdd_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 poids_cales = TRUE,
                                 stratifie = TRUE,
                                 tour = tour,
                                 strate_var = "CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_3",
                                 comment_cube = FALSE)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cubestrat_cale5, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cubestrat_filosofi2017_cale3",
        estimation = estimation
      )
    }
  }
  
  # 7. Tirage cube stratifié idf avec calage sur 9 clusters filosofi x 2017
  if (cubestrat_filosofi2017_cale8) {
    
    ech_cubestrat_cale9 <- tirage_cube(bdd_sondage = bdd_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 poids_cales = TRUE,
                                 stratifie = TRUE,
                                 tour = tour,
                                 strate_var = "CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_8",
                                 comment_cube = FALSE)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cubestrat_cale9, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cubestrat_filosofi2017_cale8",
        estimation = estimation
      )
    }
  }
  
  # Combiner tous les résultats
  return(do.call(rbind, resultats))
}
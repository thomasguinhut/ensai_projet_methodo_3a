executer_tous_plans <- function(bdd_sondage,
                                nb_bv_tires,
                                nb_max_bulletins_tires,
                                tour = "T1",
                                simple = TRUE,
                                simple_cale = TRUE,
                                stratfilosofi_cale = TRUE,
                                stratfilosofi2017_cale = TRUE,
                                cube_filosofi2017_cale= TRUE,
                                cubestrat_filosofi2017_cale = TRUE,
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
  cat("-- simple\n")
    
  # 2. Tirage simple avec calage sur 5 clusters filosofi
  if (simple_cale) {

    ech_simple_cale <- tirage_simple(bdd_sondage = bdd_sondage,
                                     nb_bv_tires = nb_bv_tires,
                                     nb_max_bulletins_tires = nb_max_bulletins_tires,
                                     poids_cales = TRUE,
                                     strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_8",
                                     type_calage = "poststrat",
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
  cat("-- simple_cale\n")
  
  # 3. Tirage stratifié filosofi avec calage sur 6 clusters filosofi
  if (stratfilosofi_cale) {

    ech_stratfilosofi_cale <- tirage_stratifie(bdd_sondage = bdd_sondage,
                                               nb_bv_tires = nb_bv_tires,
                                               nb_max_bulletins_tires = nb_max_bulletins_tires,
                                               type_strat = "egal",
                                               prez2017 = FALSE,
                                               filosofi = TRUE,
                                               nb_clusters = "6",
                                               poids_cales = TRUE,
                                               type_calage = "poststrat",
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
  cat("-- stratfilosofi_cale\n")
  
  # 4. Tirage stratifié filosofi2017 avec calage sur 8 clusters filosofi x 2017
  if (stratfilosofi2017_cale) {

    ech_stratfilosofi2017_cale <- tirage_stratifie(bdd_sondage = bdd_sondage,
                                                   nb_bv_tires = nb_bv_tires,
                                                   nb_max_bulletins_tires = nb_max_bulletins_tires,
                                                   type_strat = "egal",
                                                   prez2017 = TRUE,
                                                   filosofi = TRUE,
                                                   nb_clusters = "8",
                                                   poids_cales = TRUE,
                                                   type_calage = "poststrat",
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
  cat("-- stratfilosofi2017_cale\n")
  
  # 5. Tirage cube avec calage sur 8 clusters filosofi x 2017
  if (cube_filosofi2017_cale) {

    ech_cube_cale <- tirage_cube(bdd_sondage = bdd_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 poids_cales = TRUE,
                                 stratifie = FALSE,
                                 tour = tour,
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
  cat("-- cube_filosofi2017_cale\n")
  
  # 6. Tirage cube stratifié idf avec calage sur 8 clusters filosofi x 2017
  if (cubestrat_filosofi2017_cale) {
    
    ech_cubestrat_cale <- tirage_cube(bdd_sondage = bdd_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 poids_cales = TRUE,
                                 stratifie = TRUE,
                                 tour = tour,
                                 strate_var = "REG",
                                 comment_cube = FALSE)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cubestrat_cale, candidat)
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cubestrat_filosofi2017_cale",
        estimation = estimation
      )
    }
  }
  cat("-- cubestrat_filosofi2017_cale\n")
  
  # Combiner tous les résultats
  return(do.call(rbind, resultats))
}

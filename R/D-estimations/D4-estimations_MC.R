executer_tous_plans <- function(base_sondage,
                                nb_bv_tires,
                                nb_max_bulletins_tires,
                                type_strat,
                                strate_var,
                                tour = "T1",
                                simple = TRUE,
                                stratfilosofi = TRUE,
                                stratfilosofi2017 = TRUE,
                                cube = TRUE,
                                cubestrat = TRUE,
                                candidats = c("MACRON", "LEPEN", "MELENCHON")) {
  
  resultats <- list()
  
  # 1. Tirage simple
  if (simple) {
    # Sans calage
    ech_simple <- tirage_simple(base_sondage = base_sondage,
                                nb_bv_tires = nb_bv_tires,
                                nb_max_bulletins_tires = nb_max_bulletins_tires,
                                poids_cales = FALSE,
                                tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_simple, candidat, "simple")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "simple",
        calage = FALSE,
        estimation = estimation
      )
    }
    
    # Avec calage
    ech_simple_cale <- tirage_simple(base_sondage = base_sondage,
                                     nb_bv_tires = nb_bv_tires,
                                     nb_max_bulletins_tires = nb_max_bulletins_tires,
                                     poids_cales = TRUE,
                                     tour = tour,
                                     strate_var = strate_var)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_simple_cale, candidat, "simple")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "simple",
        calage = TRUE,
        estimation = estimation
      )
    }
  }
  
  # 2. Tirage stratifié filosofi
  if (stratfilosofi) {
    # Avec calage
    ech_stratfilosofi_cale <- tirage_stratifie(base_sondage = base_sondage,
                                               nb_bv_tires = nb_bv_tires,
                                               nb_max_bulletins_tires = nb_max_bulletins_tires,
                                               type_strat = type_strat,
                                               annee2017 = FALSE,
                                               filosofi = TRUE,
                                               nb_clusters = "8",
                                               poids_cales = TRUE,
                                               tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_stratfilosofi_cale, candidat, "stratfilosofi")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "stratfilosofi",
        calage = TRUE,
        estimation = estimation
      )
    }
  }
  
  # 3. Tirage stratifié filosofi2017
  if (stratfilosofi2017) {
    # Avec calage
    ech_stratfilosofi2017_cale <- tirage_stratifie(base_sondage = base_sondage,
                                                   nb_bv_tires = nb_bv_tires,
                                                   nb_max_bulletins_tires = nb_max_bulletins_tires,
                                                   type_strat = type_strat,
                                                   annee2017 = TRUE,
                                                   filosofi = TRUE,
                                                   nb_clusters = "9",
                                                   poids_cales = TRUE,
                                                   tour = tour)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_stratfilosofi2017_cale, candidat, "stratfilosofi2017")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "stratfilosofi2017",
        calage = TRUE,
        estimation = estimation
      )
    }
  }
  
  # 4. Tirage cube
  if (cube) {
    # Avec calage
    ech_cube_cale <- tirage_cube(base_sondage = base_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 type_strat,
                                 poids_cales = TRUE,
                                 stratifie = FALSE,
                                 tour = tour,
                                 strate_var = strate_var)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cube_cale, candidat, "cube")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cube",
        calage = TRUE,
        estimation = estimation
      )
    }
  }
  
  # 5. Tirage cube stratifié
  if (cubestrat) {
    ech_cubestrat <- tirage_cube(base_sondage = base_sondage,
                                 nb_bv_tires = nb_bv_tires,
                                 nb_max_bulletins_tires = nb_max_bulletins_tires,
                                 type_strat,
                                 poids_cales = TRUE,
                                 stratifie = TRUE,
                                 tour = tour,
                                 strate_var = strate_var)
    for (candidat in candidats) {
      estimation <- estimation_brute(ech_cubestrat, candidat, "cubestrat")
      resultats[[length(resultats) + 1]] <- data.frame(
        candidat = candidat,
        methode = "cubestrat",
        calage = TRUE,
        estimation = estimation
      )
    }
  }
  
  # Combiner tous les résultats
  return(do.call(rbind, resultats))
}
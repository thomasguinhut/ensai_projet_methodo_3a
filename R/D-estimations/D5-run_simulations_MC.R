run_simulations_MC <- function(
    nb_sim                    = 100,
    nb_bv_tires               = 600,
    nb_max_bulletins_tires    = 100,
    tour                      = "T1",
    candidats                 = c("MACRON", "LEPEN", "MELENCHON"),
    simple                    = TRUE,
    simple_cale               = TRUE,
    stratfilosofi_cale        = TRUE,
    stratfilosofi2017_cale    = TRUE,
    cube_filosofi2017_cale    = TRUE,
    cubestrat_filosofi2017_cale = TRUE,
    bdd_sondage               = base_sondage,
    s3_bucket                 = "projet-ensai-methodo-3a",
    s3_object                 = NULL
) {
  
  duree_estimee <- nb_sim * 1.2
  cat("Durée estimée :", round(duree_estimee, 1), "minutes (~",
      round(duree_estimee / 60, 1), "heures)\n\n")
  
  debut_total <- Sys.time()
  cat("Début des simulations :", nb_sim, "itérations\n")
  
  res <- lapply(X = 1:nb_sim, FUN = function(i) {
    cat("Simulation", i, "/", nb_sim, "\n")
    
    resultats <- executer_tous_plans(
      bdd_sondage                 = bdd_sondage,
      nb_bv_tires                 = nb_bv_tires,
      nb_max_bulletins_tires      = nb_max_bulletins_tires,
      tour                        = tour,
      simple                      = simple,
      simple_cale                 = simple_cale,
      stratfilosofi_cale          = stratfilosofi_cale,
      stratfilosofi2017_cale      = stratfilosofi2017_cale,
      cube_filosofi2017_cale      = cube_filosofi2017_cale,
      cubestrat_filosofi2017_cale = cubestrat_filosofi2017_cale,
      candidats                   = candidats
    )
    resultats$simulation <- i
    return(resultats)
  })
  
  res_final <- Reduce(f = rbind, x = res)
  
  if (is.null(s3_object)) {
    s3_object <- paste0(
      "resultats_simulations_MC_", nb_bv_tires, "_", nb_max_bulletins_tires, ".rds"
    )
  }
  
  aws.s3::s3write_using(
    res_final,
    FUN    = function(data, file) saveRDS(data, file = file),
    object = s3_object,
    bucket = s3_bucket,
    opts   = list(region = "")
  )
  
  duree_totale <- difftime(Sys.time(), debut_total, units = "mins")
  cat("\nTerminé en", round(duree_totale, 1), "minutes\n")
  cat("Résultats sauvegardés :", s3_object, "\n")
  
  return(res_final)
}
#' Configure et charge l'environnement R avec renv
#' 
#' Cette fonction initialise ou restaure un environnement renv, installe les packages
#' manquants, met à jour le snapshot automatiquement, et charge tous les packages.
#' 
#' @param packages_requis Vecteur de noms de packages à installer et charger
#' 
#' @examples
#' chargement_packages(c("dplyr", "ggplot2", "data.table"))

chargement_packages <- function(packages_requis) {
  
  # Configuration du chemin de bibliothèque et désactivation des prompts interactifs
  .libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
  options(
    renv.config.sandbox.enabled = FALSE,
    ask = FALSE,
    install.packages.check.source = "no"
  )
  
  # Installation de renv si nécessaire
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installation de renv...\n")
    install.packages("renv", quiet = TRUE)
  }
  
  # Branche 1: Si renv.lock existe, on restaure et on complète
  if (file.exists("renv.lock")) {
    cat("Restauration de l'environnement renv...\n")
    
    sink("/dev/null")
    renv::restore(prompt = FALSE)
    sink()
    
    # Détection des packages manquants par rapport à la liste requise
    cat("Vérification des nouveaux packages...\n")
    installed_in_renv <- rownames(installed.packages(lib.loc = renv::paths$library()))
    missing_pkgs <- setdiff(packages_requis, installed_in_renv)
    
    # Installation et mise à jour du snapshot si nécessaire
    if (length(missing_pkgs) > 0) {
      cat("⚠️ Nouveaux packages détectés :", paste(missing_pkgs, collapse = ", "), "\n")
      cat("Installation automatique...\n")
      
      sink("/dev/null")
      renv::install(missing_pkgs, prompt = FALSE)
      sink()
      
      cat("Mise à jour du snapshot renv.lock...\n")
      renv::snapshot(prompt = FALSE)
      cat("✅ Snapshot mis à jour\n")
    }
    
    # Branche 2: Si renv.lock n'existe pas, on initialise tout
  } else {
    cat("Initialisation du projet avec renv...\n")
    renv::init(bare = TRUE, restart = FALSE, 
               settings = list(snapshot.type = "implicit"))
    
    cat("Installation des packages requis...\n")
    sink("/dev/null")
    renv::install(packages_requis, prompt = FALSE)
    sink()
    
    cat("Création du snapshot renv.lock...\n")
    renv::snapshot(prompt = FALSE)
  }
  
  # Vérification finale: tous les packages doivent être présents
  cat("Vérification de la présence de tous les packages nécessaires au projet...\n")
  installed_final <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing_final <- setdiff(packages_requis, installed_final)
  
  if (length(missing_final) > 0) {
    stop(
      "❌ Erreur : Packages manquants après installation : ",
      paste(missing_final, collapse = ", ")
    )
  }
  
  # Chargement des packages avec affichage des versions
  cat("Chargement des packages...\n")
  invisible(lapply(packages_requis, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("✓ %s, version: %s\n", pkg, packageVersion(pkg)))
  }))
  
  cat("✅ Environnement de travail prêt\n")
  
  # Nettoyage de l'environnement global (garde seulement les données existantes)
  objects_to_keep <- setdiff(ls(envir = .GlobalEnv), 
                             c("chargement_packages", "packages_requis"))
  rm(list = setdiff(ls(envir = .GlobalEnv), objects_to_keep), 
     envir = .GlobalEnv)
}

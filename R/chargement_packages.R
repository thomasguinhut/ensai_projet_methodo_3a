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
  
  # --------------------------
  # Configuration R
  # --------------------------
  .libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
  options(
    renv.config.sandbox.enabled = FALSE,
    ask = FALSE,
    install.packages.check.source = "no"
  )
  
  # --------------------------
  # Installer renv si nécessaire
  # --------------------------
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installation de renv...\n")
    install.packages("renv", quiet = TRUE)
  }
  library(renv)
  
  # --------------------------
  # Branche 1 : renv.lock existe
  # --------------------------
  if (file.exists("renv.lock")) {
    cat("Restauration de l'environnement renv...\n")
    suppressMessages(renv::restore(prompt = FALSE))
    
    # Vérification des packages manquants
    lock_content <- jsonlite::fromJSON("renv.lock")
    packages_in_lock <- names(lock_content$Packages)
    missing_from_lock <- setdiff(packages_requis, packages_in_lock)
    
    if (length(missing_from_lock) > 0) {
      cat("⚠️ Nouveaux packages détectés :", paste(missing_from_lock, collapse = ", "), "\n")
      cat("Installation automatique...\n")
      suppressMessages(renv::install(missing_from_lock, prompt = FALSE))
      
      # Mise à jour du lockfile
      renv::snapshot(packages = missing_from_lock, prompt = FALSE)
      cat("✅ Packages enregistrés dans renv.lock\n")
    }
    
    # --------------------------
    # Branche 2 : renv.lock absent
    # --------------------------
  } else {
    cat("Initialisation du projet avec renv...\n")
    suppressMessages(
      renv::init(bare = TRUE, restart = FALSE, settings = list(snapshot.type = "implicit"))
    )
    
    # Installation des packages requis
    cat("Installation des packages requis...\n")
    suppressMessages(renv::install(packages_requis, prompt = FALSE))
    
    # Création du lockfile
    renv::snapshot(prompt = FALSE)
    cat("✅ renv.lock créé et packages enregistrés\n")
  }
  
  # --------------------------
  # Vérification finale
  # --------------------------
  cat("Vérification de la présence de tous les packages nécessaires au projet...\n")
  installed_final <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing_final <- setdiff(packages_requis, installed_final)
  
  if (length(missing_final) > 0) {
    stop("❌ Erreur : Packages manquants après installation : ",
         paste(missing_final, collapse = ", "))
  }
  
  # --------------------------
  # Chargement des packages
  # --------------------------
  cat("Chargement des packages...\n")
  invisible(lapply(packages_requis, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("✓ %s, version: %s\n", pkg, packageVersion(pkg)))
  }))
  
  cat("✅ Environnement de travail prêt\n")
}

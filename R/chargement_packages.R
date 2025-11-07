# -------------------------------------------------------------------
# Script non interactif pour installer les packages nécessaires
# dans un environnement RStudio Onyxia, avec gestion renv
# Vérification automatique des packages manquants
# -------------------------------------------------------------------

# 1. Définir un répertoire de bibliothèque personnalisé pour l'utilisateur
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))

# Désactiver certaines options interactives et ajuster la configuration de renv
options(
  renv.config.sandbox.enabled = FALSE,   # Désactive le mode sandbox
  ask = FALSE,                           # Désactive les invites interactives
  install.packages.check.source = "no"   # Ne pas vérifier la source des packages
)

# 2. Liste des packages nécessaires
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr")

# 3. Vérifier si renv est installé, sinon l'installer
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installation de renv...\n")
  install.packages("renv", quiet = TRUE)
}

# 4. Vérification si renv.lock existe et restaurer l'environnement, sinon on initialise
if (file.exists("renv.lock")) {
  cat("Restauration de l'environnement renv...\n")
  sink("/dev/null")                      # Redirige la sortie vers nowhere
  renv::restore(prompt = FALSE)
  sink()                                 # Rétablit la sortie standard
} else {
  # 4.1. Initialiser renv AVANT d'installer les packages
  cat("Initialisation du projet avec renv...\n")
  renv::init(bare = TRUE, restart = FALSE, settings = list(snapshot.type = "implicit"))
  
  # 4.2. Installer les packages dans l'environnement renv (sans interaction et sans messages)
  cat("Installation des packages requis dans renv...\n")
  sink("/dev/null")                      # Redirige la sortie vers nowhere
  renv::install(packages_requis, prompt = FALSE)
  sink()                                 # Rétablit la sortie standard
  
  # 4.3. Prendre un snapshot des packages installés (sans interaction)
  cat("Création du snapshot des packages...\n")
  renv::snapshot(prompt = FALSE)
}

# 5. Vérification automatique des packages manquants
cat("Vérification des dépendances...\n")
sink("/dev/null")                      # Redirige la sortie vers nowhere
deps <- renv::dependencies()
sink()                                 # Rétablit la sortie standard
installed_pkgs <- deps$Package
missing_pkgs <- setdiff(packages_requis, installed_pkgs)

if (length(missing_pkgs) > 0) {
  stop(
    "❌ Erreur : Les packages suivants sont manquants ou non installés : ",
    paste(missing_pkgs, collapse = ", "),
    "\nVérifiez renv.lock ou réinstallez-les avec `renv::install(c(\"",
    paste(missing_pkgs, collapse = "\", \""),
    "\"))`.\n"
  )
}

# 6. Chargement des packages avec version
cat("Chargement des packages...\n")
invisible(lapply(packages_requis, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  package_version <- packageVersion(pkg)
  cat(paste0("✓ ", pkg, ", version: ", package_version, "\n"))
}))
cat("✅ Environnement prêt\n")

rm(list = setdiff(ls(), "bv_2022_final"))
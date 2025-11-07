# -------------------------------------------------------------------
# Script optimisé pour réutiliser les packages déjà installés dans Onyxia
# -------------------------------------------------------------------

# 1. Configuration de l'environnement
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
options(
  renv.config.sandbox.enabled = FALSE,
  ask = FALSE,
  install.packages.check.source = "no"
)

# 2. Chargement de renv
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installation de renv...\n")
  install.packages("renv", quiet = TRUE)
}

# 3. Initialisation de renv (uniquement si renv.lock n'existe pas)
if (!file.exists("renv.lock")) {
  cat("Initialisation du projet avec renv...\n")
  renv::init(bare = TRUE, restart = FALSE, prompt = FALSE)
} else {
  cat("Restauration de l'environnement renv...\n")
  # Vérifier si renv/library/ est vide
  if (!dir.exists("renv/library") || length(list.dirs("renv/library", recursive = FALSE)) == 0) {
    sink("/dev/null")
    renv::restore(prompt = FALSE)
    sink()
  } else {
    cat("Les packages sont déjà installés dans renv/. Passage à la suite.\n")
  }
}

# 4. Liste des packages nécessaires
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr")

# 5. Fonction pour installer/charger un package (si nécessaire)
install_and_load <- function(pkg) {
  # Vérifier si le package est déjà chargé ou disponible
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("→ Installation du package : ", pkg, "\n"))
    sink("/dev/null")
    renv::install(pkg, prompt = FALSE)
    sink()
  }
  # Charger le package
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  cat(paste0("✓ ", pkg, ", version: ", packageVersion(pkg), "\n"))
}

# 6. Installer et charger les packages
cat("Chargement des packages...\n")
invisible(lapply(packages_requis, install_and_load))

# 7. Snapshot renv (si nouveau projet)
if (!file.exists("renv.lock")) {
  cat("Création du snapshot des packages...\n")
  renv::snapshot(prompt = FALSE)
}

cat("✅ Environnement prêt\n")
rm(list = setdiff(ls(), "bv_2022_final"))

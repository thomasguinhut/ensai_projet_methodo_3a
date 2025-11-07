# -------------------------------------------------------------------
# Script non interactif pour installer les packages nécessaires
# dans un environnement RStudio Onyxia, avec gestion renv
# Optimisé pour éviter les réinstallations inutiles
# -------------------------------------------------------------------

# 1. Configuration de l'environnement
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
options(
  renv.config.sandbox.enabled = FALSE,
  ask = FALSE,
  install.packages.check.source = "no"
)

# 2. Liste des packages nécessaires
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr")

# 3. Vérifier si renv est installé, sinon l'installer
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installation de renv...\n")
  install.packages("renv", quiet = TRUE)
}

# 4. Initialisation ou restauration de renv
if (file.exists("renv.lock")) {
  cat("Restauration de l'environnement renv...\n")
  sink("/dev/null")
  renv::restore(prompt = FALSE)
  sink()
} else {
  cat("Initialisation du projet avec renv...\n")
  renv::init(bare = TRUE, restart = FALSE, settings = list(snapshot.type = "implicit"))
}

# 5. Fonction pour installer/charger un package (si nécessaire)
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("→ Installation du package : ", pkg, "\n"))
    sink("/dev/null")
    renv::install(pkg, prompt = FALSE)
    sink()
  }
  # Charger le package (même s'il était déjà installé)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  cat(paste0("✓ ", pkg, ", version: ", packageVersion(pkg), "\n"))
}

# 6. Installer et charger les packages
cat("Chargement des packages...\n")
invisible(lapply(packages_requis, install_if_missing))

# 7. Snapshot renv (si nouveau projet)
if (!file.exists("renv.lock")) {
  cat("Création du snapshot des packages...\n")
  renv::snapshot(prompt = FALSE)
}

cat("✅ Environnement prêt\n")
rm(list = setdiff(ls(), "bv_2022_final"))

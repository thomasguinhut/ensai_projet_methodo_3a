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

# 2. Chargement (ou installation) de renv
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installation de renv...\n")
  install.packages("renv", quiet = TRUE)
}
suppressMessages(suppressPackageStartupMessages({
  library(renv)
  options(
    renv.config.restore.prompt = FALSE,
    renv.consent = TRUE
  )
  Sys.setenv(RENV_FORCE_PROMPT = "FALSE")
}))

# 3. Initialisation ou restauration de renv
if (file.exists("renv.lock")) {
  cat("Restauration de l'environnement renv...\n")
  sink("/dev/null")  # Masquer les logs de restauration
  renv::restore(prompt = FALSE)
  sink()
} else {
  cat("Initialisation du projet avec renv...\n")
  renv::init(bare = TRUE, restart = FALSE, prompt = FALSE)
}

# 4. Liste des packages nécessaires
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr")

# 5. Fonction pour installer/charger un package (si nécessaire)
install_and_load <- function(pkg) {
  # Installer le package s'il manque (via renv pour la reproductibilité)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("→ Installation du package : ", pkg, "\n"))
    sink("/dev/null")  # Masquer les logs d'installation
    renv::install(pkg, prompt = FALSE)
    sink()
  }
  
  # Charger le package
  if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
    cat(paste0("⚠️ Impossible de charger le package : ", pkg, "\n"))
  } else {
    cat(paste0("✓ ", pkg, ", version: ", packageVersion(pkg), "\n"))
  }
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
rm(list = setdiff(ls(), "bv_2022_final"))  # Nettoyage (à adapter selon tes besoins)

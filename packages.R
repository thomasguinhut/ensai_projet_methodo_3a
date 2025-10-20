# Liste des packages nécessaires
packages <- c(
  "dplyr"
)

# Fonction corrigée pour installer et charger les packages
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installation du package:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    cat("Package", pkg, "déjà installé et chargé\n")
  }
}

# Appliquer la fonction à chaque package
invisible(sapply(packages, install_and_load))

rm(packages, install_and_load)

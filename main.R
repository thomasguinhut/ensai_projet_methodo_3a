################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################



# Définition des packages
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl",
                     "stringr", "shiny", "FactoMineR", "factoextra", "stats")

# Chargement de pacman
if (!"pacman" %in% installed.packages()) {
  install.packages("pacman")
}
library(pacman)

# Chargement via le vecteur
pacman::p_load(char = packages_requis)




################################################################################
############################ IMPORTATION DES DONNÉES ###########################
################################################################################


source("R/A-preparation_donnees/A-bv_2022.R")


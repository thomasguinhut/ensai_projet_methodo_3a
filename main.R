################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################

# Importation de la fonction
source("R/chargement_packages.R")

# Définition des packages
packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl", 
                     "stringr", "shiny")

chargement_packages(packages_requis)


################################################################################
############################ IMPORTATION DES DONNÉES ###########################
################################################################################

source("R/A-preparation_donnees/A-bv_2022.R")


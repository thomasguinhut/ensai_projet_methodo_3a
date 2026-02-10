################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################


packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl",
                     "stringr", "shiny", "FactoMineR", "factoextra", "stats", 
                     "lwgeom", "viridis", "RColorBrewer", "ggtext", "ggrepel",
                     "gtsummary", "sampling", "survey", "fastcluster",
                     "gtsummary", "purrr")

if (!"pacman" %in% installed.packages()) {
  install.packages("pacman")
}
library(pacman)

pacman::p_load(char = packages_requis)

rm(packages_requis)



################################################################################
############################ IMPORTATION DES DONNÉES ###########################
################################################################################


source("R/A-preparation_donnees/A-bv_2022.R")

glimpse(bv_2022_final)

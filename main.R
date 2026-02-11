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
#################### IMPORTATION DES DONNÉES ET DES FONCTIONS ##################
################################################################################


source("R/A-preparation_donnees/A-bv_2022.R")
glimpse(bv_2022_final)

source("R/C-echantillonnage/C0-import_fonctions.R")
source("R/D-estimations/D0-import_fonctions.R")
source("R/Z-autres_fonctions/D0-import_fonctions.R")

base_sondage <- bv_2022_final %>% 
  filter(TIRABLE,
         INSCRITS_T1 > 99)



################################################################################
################################ ESTIMATIONS FLASH #############################
################################################################################


ech_inegal_t1 <- tirage_inegal(500, 100, FALSE, "T1")
estimation_flash(ech_inegal_t1, "MACRON", "inegal", "T1")
estimation_flash(ech_inegal_t1, "MACRON", "inegal")

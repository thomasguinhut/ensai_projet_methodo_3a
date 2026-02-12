################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################


packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl",
                     "stringr", "shiny", "FactoMineR", "factoextra", "stats", 
                     "lwgeom", "viridis", "RColorBrewer", "ggtext", "ggrepel",
                     "gtsummary", "sampling", "survey", "fastcluster",
                     "purrr", "tidyverse")

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
source("R/Z-autres_fonctions/Z0-import_fonctions.R")

base_sondage <- bv_2022_final %>% 
  filter(TIRABLE)



################################################################################
################################ ESTIMATIONS FLASH #############################
################################################################################


ech_inegal_t1 <- tirage_inegal(500, 400, TRUE, "T1")
ech_inegal_t1_cale <- ech_inegal_t1 %>% 
  mutate(poids_inegal = calage(ech_inegal_t1, ech_inegal_t1$poids_inegal))
estimation_flash(ech_inegal_t1, "MACRON", "inegal", "T1")
estimation_flash(ech_inegal_t1_cale, "MACRON", "inegal", "T1")
estimation_flash(ech_inegal_t1, "LEPEN", "inegal", "T1")
estimation_flash(ech_inegal_t1_cale, "LEPEN", "inegal", "T1")
estimation_flash(ech_inegal_t1, "MELENCHON", "inegal", "T1")
estimation_flash(ech_inegal_t1_cale, "MELENCHON", "inegal", "T1")

ech_strat_t1 <- tirage_stratifie(500, 400, FALSE, "8", TRUE, FALSE, "T1")
estimation_flash(ech_strat_t1, "MACRON", "stratfilosofi", "T1")
estimation_flash(ech_strat_t1, "LEPEN", "stratfilosofi", "T1")
estimation_flash(ech_strat_t1, "MELENCHON", "stratfilosofi", "T1")

ech_strat_2017_t1 <- tirage_stratifie_vote_prec(500, 200, "T1")
ech_strat_2017_t1_cale <- ech_strat_2017_t1 %>% 
  mutate(poids_stratfilosofi2017 = calage(ech_strat_2017_t1, 
                                          ech_strat_2017_t1$poids_stratfilosofi2017))
estimation_flash(ech_strat_2017_t1, "MACRON", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1_cale, "MACRON", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1, "LEPEN", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1_cale, "LEPEN", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1, "MELENCHON", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1_cale, "MELENCHON", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1, "ZEMMOUR", "stratfilosofi2017", "T1")
estimation_flash(ech_strat_2017_t1_cale, "ZEMMOUR", "stratfilosofi2017", "T1")

ech_cube_t1 <- tirage_cube(500, 100, FALSE, FALSE, "T1")
ech_cube_t1_cale <- ech_cube_t1 %>% 
  mutate(poids_cube = calage(ech_cube_t1, ech_cube_t1$poids_cube))
estimation_flash(ech_cube_t1, "MACRON", "cube", "T1")
estimation_flash(ech_cube_t1_cale, "MACRON", "cube", "T1")
estimation_flash(ech_cube_t1, "LEPEN", "cube", "T1")
estimation_flash(ech_cube_t1_cale, "LEPEN", "cube", "T1")
estimation_flash(ech_cube_t1, "MELENCHON", "cube", "T1")
estimation_flash(ech_cube_t1_cale, "MELENCHON", "cube", "T1")
estimation_flash(ech_cube_t1, "ZEMMOUR", "cube", "T1")
estimation_flash(ech_cube_t1_cale, "ZEMMOUR", "cube", "T1")

ech_cubecale_t1 <- tirage_cube(500, 100, FALSE, TRUE, "T1")
estimation_flash(ech_cubecale_t1, "MACRON", "cubecale", "T1")
estimation_flash(ech_cubecale_t1, "LEPEN", "cubecale", "T1")
estimation_flash(ech_cubecale_t1, "MELENCHON", "cubecale", "T1")
estimation_flash(ech_cubecale_t1, "ZEMMOUR", "cubecale", "T1")

ech_cubestrat_t1 <- tirage_cube(500, 100, TRUE, FALSE, "T1")
ech_cubestrat_t1_cale <- ech_cubestrat_t1 %>% 
  mutate(poids_cubestrat <- calage(ech_cubestrat_t1, ech_cubestrat_t1$poids_cubestrat))
estimation_flash(ech_cubestrat_t1, "MACRON", "cubestrat", "T1")
estimation_flash(ech_cubestrat_t1_cale, "MACRON", "cubestrat", "T1")
estimation_flash(ech_cubestrat_t1, "LEPEN", "cubestrat", "T1")
estimation_flash(ech_cubestrat_t1_cale, "LEPEN", "cubestrat", "T1")
estimation_flash(ech_cubestrat_t1, "MELENCHON", "cubestrat", "T1")
estimation_flash(ech_cubestrat_t1, "ZEMMOUR", "cubestrat", "T1")

ech_cubestratcale_t1 <- tirage_cube(500, 100, TRUE, TRUE, "T1")
estimation_flash(ech_cubestratcale_t1, "MACRON", "cubestratcale", "T1")
estimation_flash(ech_cubestratcale_t1, "LEPEN", "cubestratcale", "T1")
estimation_flash(ech_cubestratcale_t1, "MELENCHON", "cubestratcale", "T1")
estimation_flash(ech_cubestratcale_t1, "ZEMMOUR", "cubestratcale", "T1")



################################################################################
############################ ESTIMATIONS MONTE-CARLO ###########################
################################################################################




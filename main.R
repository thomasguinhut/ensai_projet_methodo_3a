################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################


packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl",
                     "stringr", "shiny", "FactoMineR", "factoextra", "stats", 
                     "lwgeom", "viridis", "RColorBrewer", "ggtext", "ggrepel",
                     "gtsummary", "sampling", "fastcluster", "tidyverse",
                     "purrr", "tibble", "httr")

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


ech_simple <- tirage_simple(bdd_sondage = base_sondage,
                                 nb_bv_tires = 600,
                                 nb_max_bulletins_tires = 100,
                                 poids_cales = FALSE,
                                 tour = "T1")
estimation_flash(ech_simple, "MACRON", "T1")
estimation_flash(ech_simple, "LEPEN", "T1")
estimation_flash(ech_simple, "MELENCHON", "T1")

ech_simple_cale <- tirage_simple(bdd_sondage = base_sondage,
                                 nb_bv_tires = 600,
                                 nb_max_bulletins_tires = 100,
                                 poids_cales = TRUE,
                                 strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_8",
                                 type_calage = "poststrat",
                                 tour = "T1")
estimation_flash(ech_simple_cale, "MACRON", "T1")
estimation_flash(ech_simple_cale, "LEPEN", "T1")
estimation_flash(ech_simple_cale, "MELENCHON", "T1")

ech_stratfilosofi_cale <- tirage_stratifie(
  bdd_sondage = base_sondage,
  nb_bv_tires = 600,
  nb_max_bulletins_tires = 100,
  type_strat = NULL,
  prez2017 = FALSE,
  filosofi = TRUE,
  nb_clusters = "8",
  poids_cales = TRUE,
  type_calage = "poststrat",
  tour = "T1")
estimation_flash(ech_stratfilosofi_cale, "MACRON", "T1")
estimation_flash(ech_stratfilosofi_cale, "LEPEN", "T1")
estimation_flash(ech_stratfilosofi_cale, "MELENCHON", "T1")

ech_stratfilosofi2017_cale <- tirage_stratifie(
  bdd_sondage = base_sondage,
  nb_bv_tires = 600,
  nb_max_bulletins_tires = 100,
  type_strat = NULL,
  prez2017 = TRUE,
  filosofi = TRUE,
  nb_clusters = "8",
  poids_cales = TRUE,
  type_calage = "poststrat",
  tour = "T1")
estimation_flash(ech_stratfilosofi2017_cale, "MACRON", "T1")
estimation_flash(ech_stratfilosofi2017_cale, "LEPEN", "T1")
estimation_flash(ech_stratfilosofi2017_cale, "MELENCHON", "T1")

ech_cube_cale <- tirage_cube(
  bdd_sondage = base_sondage,
  nb_bv_tires = 300,
  nb_max_bulletins_tires = 100,
  poids_cales = TRUE,
  stratifie = FALSE,
  tour = "T1",
  comment_cube = TRUE,
  method_calage = "linear")
estimation_flash(ech_cube_cale, "MACRON", "T1")
estimation_flash(ech_cube_cale, "LEPEN", "T1")
estimation_flash(ech_cube_cale, "MELENCHON", "T1")

ech_cubestrat_cale <- tirage_cube(
  bdd_sondage = base_sondage,
  nb_bv_tires = 600,
  nb_max_bulletins_tires = 100,
  poids_cales = TRUE,
  stratifie = TRUE,
  tour = "T1",
  strate_var = "ANCIEN_REG",
  comment_cube = TRUE,
  method_calage = "linear")
estimation_flash(ech_cubestrat_cale, "MACRON", "T1")
estimation_flash(ech_cubestrat_cale, "LEPEN", "T1")
estimation_flash(ech_cubestrat_cale, "MELENCHON", "T1")
estimation_flash(ech_cubestrat_cale, "ZEMMOUR", "T1")
estimation_flash(ech_cubestrat_cale, "HIDALGO", "T1")
estimation_flash(ech_cubestrat_cale, "PECRESSE", "T1")
estimation_flash(ech_cubestrat_cale, "JADOT", "T1")
estimation_flash(ech_cubestrat_cale, "DUPONTAIGNAN", "T1")
estimation_flash(ech_cubestrat_cale, "POUTOU", "T1")
estimation_flash(ech_cubestrat_cale, "ARTHAUD", "T1")
estimation_flash(ech_cubestrat_cale, "ROUSSEL", "T1")
estimation_flash(ech_cubestrat_cale, "LASSALLE", "T1")

localisation_bv_tires(ech_cubestrat_cale_linear)



################################################################################
############################ ESTIMATIONS MONTE-CARLO ###########################
################################################################################


res_sim <- run_simulations_MC(
  nb_sim                      = 1000,
  nb_bv_tires                 = 600,
  nb_max_bulletins_tires      = 100,
  tour                        = "T1",
  candidats                   = c("MACRON", "LEPEN", "MELENCHON"),
  simple                      = TRUE,
  simple_cale                 = TRUE,
  stratfilosofi_cale          = TRUE,
  stratfilosofi2017_cale      = TRUE,
  cube_filosofi2017_cale      = TRUE,
  cubestrat_filosofi2017_cale = TRUE,
  bdd_sondage                 = base_sondage,
  s3_bucket                   = "projet-ensai-methodo-3a",
  s3_object                   = NULL
)

plot_resultats(res_sim, lang = "eng")

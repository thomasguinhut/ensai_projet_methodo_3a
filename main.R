################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################


packages_requis <- c("dplyr", "aws.s3", "readxl", "arrow", "readr", "ggplot2",
                     "data.table", "sf", "forcats", "leaflet", "leafgl",
                     "stringr", "shiny", "FactoMineR", "factoextra", "stats", 
                     "lwgeom", "viridis", "RColorBrewer", "ggtext", "ggrepel",
                     "gtsummary", "sampling", "fastcluster", "tidyverse",
                     "purrr")

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


ech_simple_cale <- tirage_simple(bdd_sondage = base_sondage,
                                 nb_bv_tires = 600,
                                 nb_max_bulletins_tires = 100,
                                 poids_cales = TRUE,
                                 tour = "T1")
estimation_flash(ech_simple_t1, "MACRON", "simple_cale", "T1")
estimation_flash(ech_simple_t1, "LEPEN", "simple_cale", "T1")
estimation_flash(ech_simple_t1, "MELENCHON", "simple_cale", "T1")

ech_stratfilosofi2017_cale <- tirage_stratifie(
  bdd_sondage = base_sondage,
  nb_bv_tires = 600,
  nb_max_bulletins_tires = 100,
  type_strat = "idf",
  annee2017 = TRUE,
  filosofi = TRUE,
  nb_clusters = "5",
  poids_cales = TRUE,
  tour = "T1")
estimation_flash(ech_stratfilosofi2017_cale, "MACRON", "stratfilosofi2017_cale", "T1")
estimation_flash(ech_stratfilosofi2017_cale, "LEPEN", "stratfilosofi2017_cale", "T1")
estimation_flash(ech_stratfilosofi2017_cale, "MELENCHON", "stratfilosofi2017_cale", "T1")

ech_cube_cale <- tirage_cube(
  bdd_sondage = base_sondage,
  nb_bv_tires = 600,
  nb_max_bulletins_tires = 100,
  poids_cales = TRUE,
  stratifie = FALSE,
  tour = "T1",
  strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_5",
  comment_cube = FALSE)
estimation_flash(ech_cube_cale, "MACRON", "cube_cale", "T1")
estimation_flash(ech_cube_cale, "LEPEN", "cube_cale", "T1")
estimation_flash(ech_cube_cale, "MELENCHON", "cube_cale", "T1")

ech_cubestrat_caleegal9 <- tirage_cube(
  bdd_sondage = base_sondage,
  nb_bv_tires = nb_bv_tires,
  nb_max_bulletins_tires = nb_max_bulletins_tires,
  type_strat = "egal",
  poids_cales = TRUE,
  stratifie = TRUE,
  tour = "T1",
  strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_9",
  comment_cube = FALSE)
estimation_flash(ech_cubestrat_caleegal9, "MACRON", "cubestrat_caleegal9", "T1")
estimation_flash(ech_cubestrat_caleegal9, "LEPEN", "cubestrat_caleegal9", "T1")
estimation_flash(ech_cubestrat_caleegal9, "MELENCHON", "cubestrat_caleegal9", "T1")



################################################################################
############################ ESTIMATIONS MONTE-CARLO ###########################
################################################################################


nb_sim <- 20
nb_bv_tires <- 600
nb_max_bulletins_tires <- 100
duree_estimee <- nb_sim * 3
cat("Durée estimée:",
    round(duree_estimee, 1),
    "minutes (~",
    round(duree_estimee/60, 1),
    "heures)\n\n")

debut_total <- Sys.time()
cat("Début des simulations :", nb_sim, "itérations\n")

res <- lapply(X = 1:nb_sim, FUN = function(i){
  cat("Simulation", i, "/", nb_sim, "\n")
  
  resultats <- executer_tous_plans(bdd_sondage = base_sondage,
                                   nb_bv_tires = nb_bv_tires,
                                   nb_max_bulletins_tires = nb_max_bulletins_tires,
                                   tour = "T1",
                                   simple = TRUE,
                                   simple_cale = TRUE,
                                   stratfilosofi_cale = TRUE,
                                   stratfilosofi2017_cale = TRUE,
                                   cube_filosofi2017_cale= TRUE,
                                   cubestrat_filosofi2017_caleidf5 = TRUE,
                                   cubestrat_filosofi2017_caleidf9 = TRUE,
                                   cubestrat_filosofi2017_caleegal5 = TRUE,
                                   cubestrat_filosofi2017_caleegal9 = TRUE,
                                   candidats = c("MACRON", "LEPEN", "MELENCHON"))
  resultats$simulation <- i
  
  return(resultats)
})

res_final <- Reduce(f = rbind, x = res)
aws.s3::s3write_using(
  res_final,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "resultats_simulations_MC_600_100.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

duree_totale <- difftime(Sys.time(), debut_total, units = "mins")
cat("\nTerminé en", round(duree_totale, 1), "minutes\n")
cat("Résultats sauvegardés :", "resultats_simulations_MC_600_100.rds", "\n")

plot_resultats(res_final)

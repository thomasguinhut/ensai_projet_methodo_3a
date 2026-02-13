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


ech_cubestratcale_t1 <- tirage_cube(base_sondage = base_sondage,
                                    nb_bv_tires = 600,
                                    nb_max_bulletins_tires = 100,
                                    type_strat = "egal",
                                    poids_cales = TRUE,
                                    stratifie = TRUE,
                                    tour = "T1",
                                    strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8")
estimation_flash(ech_cubestratcale_t1, "MACRON", "cubestrat", "T1")
estimation_flash(ech_cubestratcale_t1, "LEPEN", "cubestrat", "T1")
estimation_flash(ech_cubestratcale_t1, "MELENCHON", "cubestrat", "T1")
estimation_flash(ech_cubestratcale_t1, "ZEMMOUR", "cubestrat", "T1")



################################################################################
############################ ESTIMATIONS MONTE-CARLO ###########################
################################################################################


nb_sim <- 20
nb_bv_tires <- 600
nb_max_bulletins_tires <- 100
duree_estimee <- nb_sim * 1.1
cat("Durée estimée:",
    round(duree_estimee, 1),
    "minutes (~",
    round(duree_estimee/60, 1),
    "heures)\n\n")

debut_total <- Sys.time()
cat("Début des simulations :", nb_sim, "itérations\n")

res <- lapply(X = 1:nb_sim, FUN = function(i){
  cat("Simulation", i, "/", nb_sim, "\n")
  
  resultats <- executer_tous_plans(base_sondage = base_sondage,
                                   nb_bv_tires = nb_bv_tires,
                                   nb_max_bulletins_tires = nb_max_bulletins_tires,
                                   type_strat = "idf",
                                   strate_var = "CLUSTER_AFM_IDF_DENSITE_FILOSOFI_8",
                                   tour = "T1",
                                   simple = FALSE,
                                   stratfilosofi = FALSE,
                                   stratfilosofi2017 = FALSE,
                                   cube = FALSE,
                                   cubestrat = TRUE)
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
















# 
# 
# ech_inegal_t1 <- tirage_inegal(500, 400, TRUE, "T1")
# ech_inegal_t1_cale <- ech_inegal_t1 %>% 
#   mutate(poids_inegal = calage(ech_inegal_t1, ech_inegal_t1$poids_inegal))
# estimation_flash(ech_inegal_t1, "MACRON", "inegal", "T1")
# estimation_flash(ech_inegal_t1_cale, "MACRON", "inegal", "T1")
# estimation_flash(ech_inegal_t1, "LEPEN", "inegal", "T1")
# estimation_flash(ech_inegal_t1_cale, "LEPEN", "inegal", "T1")
# estimation_flash(ech_inegal_t1, "MELENCHON", "inegal", "T1")
# estimation_flash(ech_inegal_t1_cale, "MELENCHON", "inegal", "T1")


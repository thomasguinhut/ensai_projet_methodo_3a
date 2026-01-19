# source("R/A-preparation_donnees/A1-bv_2022_brut.R")
# source("R/A-preparation_donnees/A2-bv_2022_resultats.R")
# source("R/A-preparation_donnees/A3-bv_2022_reu.R")
# source("R/A-preparation_donnees/A4-bv_2022_fermetures.R")
# source("R/A-preparation_donnees/A5-communes_2022_grille_densite.R")
# source("R/A-preparation_donnees/A6-resultats_prez2017.R")

bv_2022_final <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_7.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

aws.s3::s3write_using(
  bv_2022_final,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/bv_2022_final.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

cat("✅ Base de données bv_2022_final chargée\n")

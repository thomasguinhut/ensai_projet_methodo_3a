################################################################################
############################ Importation des données ###########################
################################################################################

bv_2022_reu_1 <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "diffusion/projet_methodo_3a/bv_2022_reu.csv",
    bucket = "thomasguinhut",
    opts = list("region" = ""),
    show_col_types = FALSE
  )

glimpse(bv_2022_reu_1)

bv_2022_final_1 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_methodo_3a/bv_2022_final_1.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_1)


################################################################################
############################ Nettoyage des bases ###############################
################################################################################

unique(substr(bv_2022_reu_1$code_commune, 1, 2))

bv_2022_reu_2 <- bv_2022_reu_1 %>% 
  filter(!(substr(code_commune, 1, 2) == "97")) # On retire les Outre-mer
  
setdiff(bv_2022_reu_2$id_brut_miom, bv_2022_final_1$ID)

sum(is.na(bv_2022_reu_2$id_brut_miom))

bv_2022_reu_2 %>% 
  filter(is.na(id_brut_miom)) %>% 
  print(n = 124)

# 55 + 124 bureaux de vote sont dans bv_2022_reu_2 sans être dans
# bv_2022_final_1, ce qui est anormal. En réalité, on remarque que cela semble
# entièrement dû à un problème d'encodage de l'id du bureau de vote (absence de 4 chiffres).

# On ajoute ainsi les "0" manquants à la gauche du numéro du bureau de vote déjà
# présent, et ce pour arriver à 4 chiffes en tout après le tiret du 8.

setdiff(bv_2022_final_1$ID, bv_2022_reu_2$id_brut_miom)


################################################################################
################################ Fusion ########################################
################################################################################

bv_2022_final_2 <- bv_2022_final_1

glimpse(bv_2022_final_2)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_2,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_methodo_3a/bv_2022_final_2.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

rm(list = ls())

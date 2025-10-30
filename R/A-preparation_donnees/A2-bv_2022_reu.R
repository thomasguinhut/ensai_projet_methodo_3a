################################################################################
############################ Importation des données ###########################
################################################################################

bv_2022_reu_1 <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "diffusion/projet_methodo_3a/bv_reu_2022.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(bv_2022_reu_1)

bv_2022_final_1 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_methodo_3a/bv_2022_1.rds",
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
setdiff(bv_2022_final_1$ID, bv_2022_reu_2$id_brut_miom)

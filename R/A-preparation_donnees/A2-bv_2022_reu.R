################################################################################
############################ Importation des données ###########################
################################################################################

bv_2022_reu_1 <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "projet-ensai-methodo-3a/sources/bv_2022_reu.csv",
    bucket = "thomasguinhut",
    opts = list("region" = ""),
    show_col_types = FALSE
  )

glimpse(bv_2022_reu_1)

bv_2022_final_1 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "projet-ensai-methodo-3a/export_bv_finaux/bv_2022_final_1.rds",
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
  dplyr::select(id_brut_reu, id_brut_insee, code, commune_reu, id_brut_miom) %>% 
  print(n = 124)

# 55 + 124 bureaux de vote sont dans bv_2022_reu_2 sans être dans
# bv_2022_final_1, ce qui est anormal. En réalité, on remarque que cela semble
# entièrement dû à un problème d'encodage de l'id de ces bureaux de vote dans la
# base bv_2022_reu_2.
# - pour les 55 qui n'ont pas de NA dans i_brut_miom, cela est dû à r absence
#   des 4 chiffres. Or, le code du bureau de vote est bien donné. On ajouterait
#   ainsi les "0" manquants à la gauche du numéro du bureau de vote déjà
#   présent, et ce pour arriver à 4 chiffes en tout après le tiret du 8.
# - pour les 124 qui ont un NA pour leur id_brut_miom, 123 ont pourtant bien un
#   code pour leur bureau de vote dans la variable correspondante. Comme pour
#   les 55 bureaux présentant l'autre problème, il est également nécessaire
#   d'ajouter des "0" et pas seulement de faire une simple concaténation.
#   En revanche, un bureau de vote à un NA dans la variable code : celui de 
#   la commune de Fornex. Or, il s'agit d'un bureau de vote unique. Nous lui
#   attribons ainsi le numéro 1 (c'est bien le code donné dans la base des
#   des résultats électoraux).

# Faisons ces deux corrections et voyons ensuite s'il reste des bureaux de 
# bv_2022_reu_2 qui ne sont pas présent dans bv_2022_final_1.




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
  object = "projet-ensai-methodo-3a/export_bv_finaux/bv_2022_final_2.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

rm(list = ls())

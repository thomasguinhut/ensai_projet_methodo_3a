################################################################################
############################ Importation des données ###########################
################################################################################

resultats_2022_t1_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "projet_methodo_3a/resultats_2022_t1_bv.xlsx",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(resultats_2022_t1_1)

resultats_2022_t2_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "projet_methodo_3a/resultats_2022_t2_bv.xlsx",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(resultats_2022_t2_1)


################################################################################
############################ Nettoyage des bases ###############################
################################################################################

resultats_2022_t1_2 <- resultats_2022_t1_1 %>% 
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         NOM_COM = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, NOM_COM, BV) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

resultats_2022_t2_2 <- resultats_2022_t2_1 %>% 
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         NOM_COM = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, NOM_COM, BV) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

setdiff(resultats_2022_t1_2$ID, resultats_2022_t2_2$ID)
setdiff(resultats_2022_t2_2$ID, resultats_2022_t1_2$ID)

# Aucun ID différent entre les deux jeux de données.
# Les deux ensembles, étant de même taille, couvrent le même ensemble de bureau
# de vote. On peut donc garder une seule des deux bases.


################################################################################
################################ Fusion ########################################
################################################################################

# On prend par exemple le premier tour.
bv_2022_1 <- resultats_2022_t1_2

glimpse(bv_2022_1)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_1,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "projet_methodo_3a/bv_2022_1.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

rm(list = ls())

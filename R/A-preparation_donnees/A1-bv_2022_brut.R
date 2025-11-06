################################################################################
############################ Importation des données ###########################
################################################################################

resultats_bv_2022_t1_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "sources/resultats_bv_2022_t1.xlsx",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(resultats_bv_2022_t1_1)

resultats_bv_2022_t2_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "projet-ensai-methodo-3a/sources/resultats_bv_2022_t2.xlsx",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(resultats_bv_2022_t2_1)


################################################################################
############################ Nettoyage des bases ###############################
################################################################################

resultats_bv_2022_t1_2 <- resultats_bv_2022_t1_1 %>% 
  # Sélection des variables importantes et renommage
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

resultats_bv_2022_t2_2 <- resultats_bv_2022_t2_1 %>% 
  # Sélection des variables importantes et renommage
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

setdiff(resultats_bv_2022_t1_2$ID, resultats_bv_2022_t2_2$ID)
setdiff(resultats_bv_2022_t2_2$ID, resultats_bv_2022_t1_2$ID)

# Aucun ID différent quand on compare les deux jeux de données.
# Les deux ensembles, étant également de même taille, couvrent le même ensemble
# de bureau de vote. On peut donc garder une seule des deux bases.
bv_2022_final_1 <- resultats_bv_2022_t1_2

glimpse(bv_2022_final_1)



################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_1,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "projet-ensai-methodo-3a/export_bv_finaux/bv_2022_final_1.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

rm(list = ls())

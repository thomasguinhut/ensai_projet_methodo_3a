################################################################################
############################ Importation des données ###########################
################################################################################

objets_initiaux <- ls()

resultats_bv_2022_t1_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "/sources/resultats_bv_2022_t1.xlsx",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(resultats_bv_2022_t1_1)

resultats_bv_2022_t2_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "/sources/resultats_bv_2022_t2.xlsx",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(resultats_bv_2022_t2_1)

bv_2022_final_1 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_1.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_1)


################################################################################
############################ Nettoyage des bases ###############################
################################################################################

resultats_bv_2022_t1_2 <- resultats_bv_2022_t1_1 %>% 
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                Inscrits, Votants, Exprimés, Voix, ...33, ...40, ...47, ...54,
                ...61, ...68, ...75, ...82, ...89, ...96, ...103,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`),
         Inscrits = as.numeric(Inscrits)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         INSCRITS_T1 = Inscrits,
         VOTANTS_T1 = Votants,
         EXPRIMES_T1 = Exprimés,
         ARTHAUD_T1 = Voix,
         ROUSSEL_T1 = ...33,
         MACRON_T1 = ...40,
         LASSALLE_T1 = ...47,
         LEPEN_T1 = ...54,
         ZEMMOUR_T1 = ...61,
         MELENCHON_T1 = ...68,
         HIDALGO_T1 = ...75,
         JADOT_T1 = ...82,
         PECRESSE_T1 = ...89,
         POUTOU_T1 = ...96,
         DUPONTAIGNAN_T1 = ...103,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV, INSCRITS_T1, VOTANTS_T1, EXPRIMES_T1,
                ARTHAUD_T1, ROUSSEL_T1, MACRON_T1, LASSALLE_T1, LEPEN_T1,
                ZEMMOUR_T1, MELENCHON_T1, HIDALGO_T1, JADOT_T1, JADOT_T1,
                PECRESSE_T1, POUTOU_T1, DUPONTAIGNAN_T1) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

resultats_bv_2022_t2_2 <- resultats_bv_2022_t2_1 %>% 
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                Inscrits, Votants, Exprimés, Voix, ...33,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         INSCRITS_T2 = Inscrits,
         VOTANTS_T2 = Votants,
         EXPRIMES_T2 = Exprimés,
         MACRON_T2 = Voix,
         LEPEN_T2 = ...33,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV, INSCRITS_T2, VOTANTS_T2, EXPRIMES_T2,
                MACRON_T2, LEPEN_T2) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ"))) # On retirer les Outre-mer

setdiff(resultats_bv_2022_t1_2$ID, resultats_bv_2022_t2_2$ID)
setdiff(resultats_bv_2022_t2_2$ID, resultats_bv_2022_t1_2$ID)

# Aucun ID différent entre les deux jeux de données.
# Les deux ensembles, étant de même taille, sont identiques.
# On peut donc les fusionner sans risque de doublons ou de pertes.


################################################################################
################################ Fusions #######################################
################################################################################

# On fusionne les résultats des deux tours des présidentielles 2022
resultats_bv_2022 <- resultats_bv_2022_t1_2 %>%
  dplyr::inner_join(
    resultats_bv_2022_t2_2 %>%
      dplyr::select(ID, INSCRITS_T2, VOTANTS_T2, EXPRIMES_T2, MACRON_T2,
                    LEPEN_T2),
    by = "ID"
  ) %>% 
  # On convertit les résultats électoraux en entiers pour être cohérent
  mutate(across(ends_with(c("T1", "T2")), as.integer))

glimpse(resultats_bv_2022)
glimpse(bv_2022_final_1)

# On fusionne avec la dernière version de la base des bureaux de vote.
bv_2022_final_2 <- bv_2022_final_1 %>%
  inner_join(resultats_bv_2022 %>% 
               dplyr::select(-c(DEP, COM, COM_LIB, BV)),  by = "ID") %>% 
  # On supprime les bureaux à 0 inscrits
  filter(!(INSCRITS_T1 == 0 | INSCRITS_T1 == 0))

glimpse(bv_2022_final_2)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_2,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_2.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

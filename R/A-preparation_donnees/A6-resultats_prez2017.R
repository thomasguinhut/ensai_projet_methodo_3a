################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

resultats_bv_2017_t1_1 <- aws.s3::s3read_using(
  FUN = fread,
  sep = ";",
  encoding = "Latin-1",
  data.table = FALSE,
  object = "sources/PR17_BVot_T1_FE.txt",
  bucket = "projet-ensai-methodo-3a",
  opts = list("region" = "")
)

glimpse(resultats_bv_2017_t1_1)

resultats_bv_2017_t2_1 <- aws.s3::s3read_using(
  FUN = fread,
  sep = ";",
  encoding = "Latin-1",
  data.table = FALSE,
  object = "sources/PR17_BVot_T2_FE.txt",
  bucket = "projet-ensai-methodo-3a",
  opts = list("region" = "")
)

glimpse(resultats_bv_2017_t2_1)

bv_2022_final_5 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_4.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_5)



################################################################################
############################ Nettoyage des bases ###############################
################################################################################


resultats_bv_2017_t1_2 <- resultats_bv_2017_t1_1 %>% 
  mutate(`Code de la commune` = str_pad(`Code de la commune`, width = 3, side = "left", pad = "0")) %>%
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                Inscrits, Votants, Exprimés, Voix, V33, V40, V47, V54,
                V61, V68, V75, V82, V89, V96,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`),
         Inscrits = as.numeric(Inscrits)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         INSCRITS_2017_T1 = Inscrits,
         VOTANTS_2017_T1 = Votants,
         EXPRIMES_2017_T1 = Exprimés,
         DUPONTAIGNAN_2017_T1 = Voix,
         LEPEN_2017_T1 = V33,
         MACRON_2017_T1 = V40,
         HAMON_2017_T1 = V47,
         ARTHAUD_2017_T1 = V54,
         POUTOU_2017_T1 = V61,
         CHEMINADE_2017_T1 = V68,
         LASSALLE_2017_T1 = V75,
         MÉLENCHON_2017_T1 = V82,
         ASSELINEAU_2017_T1 = V89,
         FILLON_2017_T1 = V96,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV, INSCRITS_2017_T1, VOTANTS_2017_T1,
                EXPRIMES_2017_T1, DUPONTAIGNAN_2017_T1, LEPEN_2017_T1, MACRON_2017_T1,
                HAMON_2017_T1, ARTHAUD_2017_T1, POUTOU_2017_T1, CHEMINADE_2017_T1,
                LASSALLE_2017_T1, MÉLENCHON_2017_T1, ASSELINEAU_2017_T1,
                FILLON_2017_T1) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ")))

resultats_bv_2017_t2_2 <- resultats_bv_2017_t2_1 %>% 
  mutate(`Code de la commune` = str_pad(`Code de la commune`, width = 3, side = "left", pad = "0")) %>%
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                Inscrits, Votants, Exprimés, Voix, V33,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`),
         Inscrits = as.numeric(Inscrits)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         INSCRITS_2017_T2 = Inscrits,
         VOTANTS_2017_T2 = Votants,
         EXPRIMES_2017_T2 = Exprimés,
         MACRON_2017_T2 = Voix,
         LEPEN_2017_T2 = V33,
         COM_LIB = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, COM_LIB, BV, INSCRITS_2017_T2, VOTANTS_2017_T2,
                EXPRIMES_2017_T2, MACRON_2017_T2, LEPEN_2017_T2) %>% 
  filter(!(DEP %in% c("ZA", "ZB", "ZC", "ZD", "ZM", "ZN", "ZP", "ZS", "ZW",
                      "ZX", "ZZ")))

setdiff(resultats_bv_2017_t1_2$ID, resultats_bv_2017_t2_2$ID)
setdiff(resultats_bv_2017_t2_2$ID, resultats_bv_2017_t1_2$ID)

# Aucun ID différent entre les deux jeux de données.
# Les deux ensembles, étant de même taille, sont identiques.
# On peut donc les fusionner sans risque de doublons ou de pertes.



################################################################################
################################ Fusions #######################################
################################################################################


# On fusionne les résultats des deux tours des présidentielles 2022
resultats_bv_2017 <- resultats_bv_2017_t1_2 %>%
  dplyr::inner_join(
    resultats_bv_2017_t2_2 %>%
      dplyr::select(ID, INSCRITS_2017_T2, VOTANTS_2017_T2, EXPRIMES_2017_T2,
                    MACRON_2017_T2, LEPEN_2017_T2),
    by = "ID"
  ) %>% 
  # On convertit les résultats électoraux en entiers pour être cohérent
  mutate(across(ends_with(c("T1", "T2")), as.integer))

glimpse(resultats_bv_2017)
glimpse(bv_2022_final_5)

length(setdiff(bv_2022_final_5$ID, resultats_bv_2017_t1_2$ID)) # 1246
length(setdiff(resultats_bv_2017_t1_2$ID, bv_2022_final_5$ID)) # 995, mais osef

bv_2022_final_6 <- bv_2022_final_5 %>%
  left_join(
    resultats_bv_2017 %>%
      select(ID, ends_with("_2017_T1"), ends_with("_2017_T2")),
    by = "ID"
  ) %>%
  mutate(NX_BV = as.integer(if_any(ends_with("_2017_T1") | ends_with("_2017_T2"), is.na)))



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_final_6,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_6.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)


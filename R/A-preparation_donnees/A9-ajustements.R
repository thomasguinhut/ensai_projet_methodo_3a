################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

bv_2022_final_8 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_8.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_8)



################################################################################
################################## Ajustement ##################################
################################################################################


bv_2022_final_9 <- bv_2022_final_8 %>%
  mutate(
    CLUSTER_AFM_IDF_DENSITE_FILOSOFI_6 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_6),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_6),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_6)
    ),
    CLUSTER_AFM_IDF_DENSITE_FILOSOFI_2017_8 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_2017_8),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_8),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_8)
    ),
    CLUSTER_AFM_IDF_DENSITE_2017_7 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_2017_7),
      paste0("idf_", CLUSTER_AFM_DENSITE_2017_7),
      paste0("hdf_", CLUSTER_AFM_DENSITE_2017_7)
    )
  ) %>% 
  mutate(
    CLUSTER_AFM_REG_DENSITE_FILOSOFI_6 = paste(REG, CLUSTER_AFM_DENSITE_FILOSOFI_6, sep = "_"),
    CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_8 = paste(REG, CLUSTER_AFM_DENSITE_FILOSOFI_2017_8, sep = "_"),
    CLUSTER_AFM_REG_DENSITE_2017_7 = paste(REG, CLUSTER_AFM_DENSITE_2017_7, sep = "_")
  ) %>% 
  mutate(
    REG_DENS3 = paste(REG, DENS3, sep = "_"),
    REG_DENS4 = paste(REG, DENS4, sep = "_"),
    REG_DENS7 = paste(REG, DENS7, sep = "_")
  )

bv_2022_final_9 <- bv_2022_final_9 %>%
  mutate(ANCIEN_REG = case_when(
    DEP %in% c("01", "03", "07", "15", "26", "38", "42", "43", "63", "69", "73", "74") ~ "Rhône-Alpes",
    DEP %in% c("03", "15", "43", "63") ~ "Auvergne",
    DEP %in% c("21", "58", "71", "89") ~ "Bourgogne",
    DEP %in% c("25", "39", "58", "70", "90") ~ "Franche-Comté",
    DEP %in% c("22", "29", "35", "56") ~ "Bretagne",
    DEP %in% c("44", "49", "53", "72", "85") ~ "Pays de la Loire",
    DEP %in% c("18", "28", "36", "37", "41", "45") ~ "Centre-Val de Loire",
    DEP %in% c("2A", "2B") ~ "Corse",
    DEP %in% c("08", "10", "51", "52") ~ "Champagne-Ardenne",
    DEP %in% c("54", "55", "57", "88") ~ "Lorraine",
    DEP %in% c("67", "68") ~ "Alsace",
    DEP %in% c("02", "60", "80") ~ "Picardie",
    DEP %in% c("59", "62") ~ "Nord-Pas-de-Calais",
    DEP %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ "Île-de-France",
    DEP %in% c("09", "12", "31", "32", "46", "65", "81", "82") ~ "Midi-Pyrénées",
    DEP %in% c("11", "30", "34", "48", "66") ~ "Languedoc-Roussillon",
    DEP %in% c("16", "17", "79", "86") ~ "Poitou-Charentes",
    DEP %in% c("19", "23", "87") ~ "Limousin",
    DEP %in% c("24", "33", "40", "47", "64") ~ "Aquitaine",
    DEP %in% c("14", "27", "50", "61", "76") ~ "Normandie",
    DEP %in% c("04", "05", "06", "13", "83", "84") ~ "Provence-Alpes-Côte d'Azur",
  )) %>%
  relocate(ANCIEN_REG, .before = REG)



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_final_9,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_9.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

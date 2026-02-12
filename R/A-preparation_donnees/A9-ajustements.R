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
    CLUSTER_AFM_DENSITE_FILOSOFI_5 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_5),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_5),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_5)
    ),
    CLUSTER_AFM_DENSITE_FILOSOFI_8 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_8),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_8),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_8)
    ),
    CLUSTER_AFM_DENSITE_FILOSOFI_2017_5 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_2017_5),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_5),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_5)
    ),
    CLUSTER_AFM_DENSITE_FILOSOFI_2017_9 = ifelse(
      REG == "11" & !is.na(CLUSTER_AFM_DENSITE_FILOSOFI_2017_9),
      paste0("idf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_9),
      paste0("hdf_", CLUSTER_AFM_DENSITE_FILOSOFI_2017_9)
    )
  )




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
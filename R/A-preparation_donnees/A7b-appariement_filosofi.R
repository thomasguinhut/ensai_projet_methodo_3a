################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

bv_2022_car <- aws.s3::s3read_using(
  FUN = read_csv2,
  object = "/appariement_carreaux_filosofi/carreau_appariement_bv3.csv",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

glimpse(bv_2022_car)

filosofi <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "/filosofi_sans_geom.parquet",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

glimpse(filosofi)

bv_2022_final_6 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_6.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_6)



################################################################################
################################ Classif #######################################
################################################################################


bv_2022_1 <- bv_2022_car %>%
  dplyr::left_join(
    filosofi,
    by = c("idcar_200m" = "idcar_200m")
  ) %>% 
  dplyr::select(-c(...1))

bv_2022_2 <- bv_2022_1 %>%
  group_by(id_brut_bv_reu) %>%
  summarise(
    # Population & ménages
    IND = as.integer(sum(ind, na.rm = TRUE)),
    MEN = as.integer(sum(men, na.rm = TRUE)),
    MEN_PAUV = round(sum(men_pauv, na.rm = TRUE) / MEN * 100, 1),
    MEN_1IND = round(sum(men_1ind, na.rm = TRUE) / MEN * 100, 1),
    MEN_5IND = round(sum(men_5ind, na.rm = TRUE) / MEN * 100, 1),
    MEN_PROP = round(sum(men_prop, na.rm = TRUE) / MEN * 100, 1),
    MEN_FMP  = round(sum(men_fmp,  na.rm = TRUE) / MEN * 100, 1),
    
    # Revenus & logement
    IND_SNV  = round(sum(ind_snv,  na.rm = TRUE) / IND * 100, 1),
    MEN_COLL = round(sum(men_coll, na.rm = TRUE) / MEN * 100, 1),
    MEN_MAIS = round(sum(men_mais, na.rm = TRUE) / MEN * 100, 1),
    
    # Logements par période
    LOG = as.integer(sum(log_av45, na.rm = TRUE) +
      sum(log_45_70, na.rm = TRUE) +
      sum(log_70_90, na.rm = TRUE) +
      sum(log_ap90, na.rm = TRUE) +
      sum(log_inc,   na.rm = TRUE)),
    LOG_SOC  = round(sum(log_soc,  na.rm = TRUE) / LOG * 100, 1),
    MEN_SURF = round(sum(men_surf, na.rm = TRUE) / LOG, 1),
    
    # Âges
    IND_0_3   = round(sum(ind_0_3,   na.rm = TRUE) / IND * 100, 1),
    IND_4_5   = round(sum(ind_4_5,   na.rm = TRUE) / IND * 100, 1),
    IND_6_10  = round(sum(ind_6_10,  na.rm = TRUE) / IND * 100, 1),
    IND_11_17 = round(sum(ind_11_17, na.rm = TRUE) / IND * 100, 1),
    IND_18_24 = round(sum(ind_18_24, na.rm = TRUE) / IND * 100, 1),
    IND_25_39 = round(sum(ind_25_39, na.rm = TRUE) / IND * 100, 1),
    IND_40_54 = round(sum(ind_40_54, na.rm = TRUE) / IND * 100, 1),
    IND_55_64 = round(sum(ind_55_64, na.rm = TRUE) / IND * 100, 1),
    IND_65_79 = round(sum(ind_65_79, na.rm = TRUE) / IND * 100, 1),
    IND_80P   = round(sum(ind_80p,   na.rm = TRUE) / IND * 100, 1),
    IND_INC   = round(sum(ind_inc,   na.rm = TRUE) / IND * 100, 1),
    
    .groups = "drop"
  )

bv_2022_3 <- bv_2022_final_6 %>%
  dplyr::left_join(
    bv_2022_2,
    by = c("ID_REU" = "id_brut_bv_reu")
  )



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_3,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_7.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

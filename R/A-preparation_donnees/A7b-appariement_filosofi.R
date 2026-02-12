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
    # Population & ménages - totaux
    IND = max(1L, as.integer(sum(ind, na.rm = TRUE))),
    MEN = max(1L, as.integer(sum(men, na.rm = TRUE))),
    MEN_PAUV = as.integer(sum(men_pauv, na.rm = TRUE)),
    MEN_1IND = as.integer(sum(men_1ind, na.rm = TRUE)),
    MEN_5IND = as.integer(sum(men_5ind, na.rm = TRUE)),
    MEN_PROP = as.integer(sum(men_prop, na.rm = TRUE)),
    MEN_FMP = as.integer(sum(men_fmp, na.rm = TRUE)),
    
    # Revenus & logement - totaux
    MEN_SNV = as.integer(sum(ind_snv, na.rm = TRUE)),
    MEN_COLL = as.integer(sum(men_coll, na.rm = TRUE)),
    MEN_MAIS = as.integer(sum(men_mais, na.rm = TRUE)),
    
    # Logements - totaux
    LOG = max(
      1L,
      as.integer(
        sum(log_av45, na.rm = TRUE) +
          sum(log_45_70, na.rm = TRUE) +
          sum(log_70_90, na.rm = TRUE) +
          sum(log_ap90, na.rm = TRUE) +
          sum(log_inc, na.rm = TRUE)
      )
    ),
    LOG_SOC = as.integer(sum(log_soc, na.rm = TRUE)),
    MEN_SURF = as.integer(sum(men_surf, na.rm = TRUE)),
    
    # Âges - totaux
    IND_0_3 = as.integer(sum(ind_0_3, na.rm = TRUE)),
    IND_4_5 = as.integer(sum(ind_4_5, na.rm = TRUE)),
    IND_6_10 = as.integer(sum(ind_6_10, na.rm = TRUE)),
    IND_11_17 = as.integer(sum(ind_11_17, na.rm = TRUE)),
    IND_18_24 = as.integer(sum(ind_18_24, na.rm = TRUE)),
    IND_25_39 = as.integer(sum(ind_25_39, na.rm = TRUE)),
    IND_40_54 = as.integer(sum(ind_40_54, na.rm = TRUE)),
    IND_55_64 = as.integer(sum(ind_55_64, na.rm = TRUE)),
    IND_65_79 = as.integer(sum(ind_65_79, na.rm = TRUE)),
    IND_80P = as.integer(sum(ind_80p, na.rm = TRUE)),
    IND_AGE_CONNU = (IND_0_3 + IND_4_5 + IND_6_10 + IND_11_17 + IND_18_24 +
                       IND_25_39 + IND_40_54 + IND_55_64 + IND_65_79 + IND_80P),
    
    # Proportions
    PROP_MEN_PAUV = round(MEN_PAUV / MEN * 100, 1),
    PROP_MEN_1IND = round(MEN_1IND / MEN * 100, 1),
    PROP_MEN_5IND = round(MEN_5IND / MEN * 100, 1),
    PROP_MEN_PROP = round(MEN_PROP / MEN * 100, 1),
    PROP_MEN_FMP = round(MEN_FMP / MEN * 100, 1),
    MOY_MEN_NV = round(MEN_SNV / IND, 1),
    PROP_MEN_COLL = round(MEN_COLL / MEN * 100, 1),
    PROP_MEN_MAIS = round(MEN_MAIS / MEN * 100, 1),
    PROP_LOG_SOC = round(LOG_SOC / LOG * 100, 1),
    MOY_MEN_SURF = round(MEN_SURF / LOG, 1),
    PROP_IND_0_3 = round(IND_0_3 / IND_AGE_CONNU * 100, 1),
    PROP_IND_4_5 = round(IND_4_5 / IND_AGE_CONNU * 100, 1),
    PROP_IND_6_10 = round(IND_6_10 / IND_AGE_CONNU * 100, 1),
    PROP_IND_11_17 = round(IND_11_17 / IND_AGE_CONNU * 100, 1),
    PROP_IND_18_24 = round(IND_18_24 / IND_AGE_CONNU * 100, 1),
    PROP_IND_25_39 = round(IND_25_39 / IND_AGE_CONNU * 100, 1),
    PROP_IND_40_54 = round(IND_40_54 / IND_AGE_CONNU * 100, 1),
    PROP_IND_55_64 = round(IND_55_64 / IND_AGE_CONNU * 100, 1),
    PROP_IND_65_79 = round(IND_65_79 / IND_AGE_CONNU * 100, 1),
    PROP_IND_80P = round(IND_80P / IND_AGE_CONNU * 100, 1),
    
    .groups = "drop"
  ) %>%
  filter(IND_AGE_CONNU != 0,
         LOG != 0,
         MEN != 0,
         IND != 0) %>% 
  dplyr::select(-IND_AGE_CONNU)

length(setdiff(bv_2022_2$id_brut_bv_reu, bv_2022_final_6$ID_REU))
length(setdiff(bv_2022_final_6$ID_REU, bv_2022_2$id_brut_bv_reu))

bv_2022_3 <- bv_2022_final_6 %>%
  inner_join(
    bv_2022_2,
    by = c("ID_REU" = "id_brut_bv_reu")
  )

bv_2022_4 <- bv_2022_3 %>% 
  filter(EXPRIMES_T1 != 0 & EXPRIMES_T2 != 0)



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_4,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_7.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

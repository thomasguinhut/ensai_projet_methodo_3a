################################################################################
############################ Importation des données ###########################
################################################################################

bv_2022_car <- aws.s3::s3read_using(
  FUN = read_csv2,
  object = "/appariement_carreaux_filosofi/carreau_appariement_bv3.csv",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

filosofi <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "/filosofi_sans_geom.parquet",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

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

bv_2022 <- bv_2022_final_6 %>%
  dplyr::left_join(
    bv_2022_car,
    by = c("ID_REU" = "id_brut_bv_reu")
  )
bv_2022 <- bv_2022 %>%
  select(-c(...1))

bv_2022 <- bv_2022 %>%
  dplyr::left_join(
    filosofi,
    by = c("idcar_200m" = "idcar_200m")
  )

bv_2022 <- bv_2022 %>%
  group_by(ID_REU) %>%
  summarise(
    # Population & ménages
    Ind = sum(ind, na.rm = TRUE),
    Men = sum(men, na.rm = TRUE),
    men_pauv = sum(men_pauv, na.rm = TRUE)/Men,
    men_1ind = sum(men_1ind, na.rm = TRUE)/Men,
    men_5ind = sum(men_5ind, na.rm = TRUE)/Men,
    men_prop = sum(men_prop, na.rm = TRUE)/Men,
    men_fmp = sum(men_fmp, na.rm = TRUE)/Men,
    
    # Revenus & logement
    ind_snv = sum(ind_snv, na.rm = TRUE)/Ind,
    men_surf = sum(men_surf, na.rm = TRUE)/Men,
    men_coll = sum(men_coll, na.rm = TRUE)/Men,
    men_mais = sum(men_mais, na.rm = TRUE)/Men,
    
    # Logements par période
    log = sum(log_av45, na.rm = TRUE) + sum(log_45_70, na.rm = TRUE) + 
      sum(log_70_90, na.rm = TRUE) + sum(log_ap90, na.rm = TRUE) + 
      sum(log_inc, na.rm = TRUE),
    log_soc = sum(log_soc, na.rm = TRUE)/log,
    
    # Âges
    ind_0_3 = sum(ind_0_3, na.rm = TRUE)/Ind,
    ind_4_5 = sum(ind_4_5, na.rm = TRUE)/Ind,
    ind_6_10 = sum(ind_6_10, na.rm = TRUE)/Ind,
    ind_11_17 = sum(ind_11_17, na.rm = TRUE)/Ind,
    ind_18_24 = sum(ind_18_24, na.rm = TRUE)/Ind,
    ind_25_39 = sum(ind_25_39, na.rm = TRUE)/Ind,
    ind_40_54 = sum(ind_40_54, na.rm = TRUE)/Ind,
    ind_55_64 = sum(ind_55_64, na.rm = TRUE)/Ind,
    ind_65_79 = sum(ind_65_79, na.rm = TRUE)/Ind,
    ind_80p = sum(ind_80p, na.rm = TRUE)/Ind,
    ind_inc = sum(ind_inc, na.rm = TRUE)/Ind,
    
    .groups = "drop"
  )

df_quant <- bv_2022 %>%
  select(where(is.numeric))
df_quant <- df_quant %>%
  select(-c(Ind, Men))

data_scaled <- scale(df_quant)

# ACP
res.acp <- PCA(data_scaled, scale.unit = TRUE, ncp = 10, graph = FALSE)

k <- 3
coord_acp <- res.acp$ind$coord[, 1:k]

set.seed(123)
kmeans_result <- kmeans(coord_acp, centers = 4, nstart = 25)

bv_2022$Cluster <- as.factor(kmeans_result$cluster)


bv_2022_final_7 <- bv_2022_final_6 %>%
  dplyr::left_join(
    bv_2022,
    by = c("ID_REU" = "ID_REU")
  )

################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_final_7,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_7.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

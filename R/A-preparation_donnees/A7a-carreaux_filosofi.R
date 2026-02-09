################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

bv_2022_final_6 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_6.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_6)

gp <- 
  aws.s3::s3read_using(
    FUN = st_read,
    object = "/sources/carreaux_200m_met.gpkg",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

adresses_df <- 
  aws.s3::s3read_using(
    FUN = read_parquet,
    object = "/sources/adresses_2022_reu.parquet",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  ) %>%
  filter(substr(code_commune_ref, 1, 2) != "97")

# Gestion des objets sf :

adresses <- st_as_sf(
  adresses_df,
  coords = c("longitude", "latitude"),  
  crs = 4326
)

carreaux <- gp %>%
  select(idcar_200m, geom)

adresses <- st_transform(adresses, st_crs(carreaux))

# Jointure bv / carreaux :

jointure <- st_join(adresses, carreaux, join = st_within)

jointure_sans_NA <- jointure %>%
  filter(!is.na(idcar_200m))

bureau_carreau <- jointure_sans_NA %>%
  select(id_brut_bv_reu, idcar_200m) 
# on garde toutes les combinaisons carreaux / bv pour faire un group by

dt <- as.data.table(bureau_carreau) # passage en datatable pour les calculs
counts <- dt[, .N, by = .(idcar_200m, id_brut_bv_reu)]

setorder(counts, idcar_200m, -N)

bv_par_carreau <- counts[, .SD[1], by = idcar_200m]
# on ne garde que le bv avec le plus grand N par carreau
# correspond au nombre d'adresses du carreaux associées au bv en question



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_par_carreau,
  FUN = readr::write_csv,
  object = "/appariement_carreaux_filosofi/carreau_appariement_bv3.csv",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

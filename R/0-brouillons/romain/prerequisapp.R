library(sf)
library(leaflet)
library(dplyr)
library(leafgl)
library(stringr)

filosofi <- aws.s3::s3read_using(
  FUN = sf::st_read,
  object = "sources/carreaux_200m_met.gpkg",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

bdv <- aws.s3::s3read_using(
  FUN = sf::st_read,
  object = "sources/contours-france-entiere-latest-v2.geojson",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)
bdv_l93 <- st_transform(bdv, 2154)

filosofi <- filosofi %>%
  mutate(dept = str_sub(lcog_geo, 1, 2)) 

dir.create("R/0-brouillons/romain/filosofi_departements", showWarnings = FALSE)

departements <- unique(filosofi$dept)

for(dep in departements){
  # Filtrer le département
  filosofi_dep <- filosofi %>% filter(dept == dep)
  
  # Sauvegarder
  st_write(filosofi_dep, paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_", dep, ".gpkg"),
           driver = "GPKG", delete_dsn = TRUE)
  
  cat("Département", dep, "terminé et sauvegardé\n")
}

dir.create("R/0-brouillons/romain/bdv_departements", showWarnings = FALSE)

departements1 <- unique(bdv_l93$codeDepartement)

for(dep in departements1){
  # Filtrer le département
  bdv_l93_dep <- bdv_l93 %>% filter(codeDepartement == dep)
  
  # Sauvegarder
  st_write(bdv_l93_dep, paste0("R/0-brouillons/romain/bdv_departements/bdv_dep_", dep, ".gpkg"),
           driver = "GPKG", delete_dsn = TRUE)
  
  cat("Département", dep, "terminé et sauvegardé\n")
}
rm(departements, departements1)

adresse <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "sources/table-adresses-reu.parquet",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)
str(adresse)

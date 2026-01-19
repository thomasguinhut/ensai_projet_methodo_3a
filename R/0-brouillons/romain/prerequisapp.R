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
filosofi_4326 <- st_transform(filosofi, 4326)
bdv_4326 <- st_transform(bdv, 4326)

filosofi_4326 <- filosofi_4326 %>%
  select(c(idcar_200m, lcog_geo, ind, men, men_pauv, ind_snv, men_prop, geom))

filosofi <- filosofi %>%
  mutate(dept = str_sub(lcog_geo, 1, 2)) 
str(filosofi_4326)

dir.create("R/0-brouillons/romain/filosofi_departements", showWarnings = FALSE)

bdv_4326 <- bdv_4326 %>%
  filter()
departements <- unique(bdv_4326$codeDepartement)
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



############################################################
# PRÉTRAITEMENT CARTOGRAPHIQUE – R PUR
# Carreaux Filosofi + Bureaux de vote
############################################################

# ======================
# 1. LIBRAIRIES
# ======================

library(sf)
library(dplyr)
library(stringr)
library(aws.s3)
library(lwgeom)

sf::sf_use_s2(FALSE)  # plus rapide pour les opérations géométriques

# ======================
# 2. PARAMÈTRES
# ======================

BUCKET <- "projet-ensai-methodo-3a"

OUT_FILOSOFI <- "data/filosofi_departements"
OUT_BDV      <- "data/bdv_departements"

dir.create(OUT_FILOSOFI, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_BDV, recursive = TRUE, showWarnings = FALSE)

# ======================
# 3. LECTURE DES DONNÉES
# ======================

message("▶ Lecture des carreaux Filosofi")

filosofi <- aws.s3::s3read_using(
  FUN = st_read,
  object = "sources/carreaux_200m_met.gpkg",
  bucket = BUCKET,
  opts = list(region = ""),
  quiet = TRUE
)

message("▶ Lecture des bureaux de vote")

bdv <- aws.s3::s3read_using(
  FUN = st_read,
  object = "sources/contours-france-entiere-latest-v2.geojson",
  bucket = BUCKET,
  opts = list(region = ""),
  quiet = TRUE
)

# ======================
# 4. CRS UNIQUE (WGS84)
# ======================

filosofi <- st_transform(filosofi, 4326)
bdv      <- st_transform(bdv, 4326)

# ======================
# 5. NETTOYAGE DES DONNÉES
# ======================

# Colonnes utiles uniquement
filosofi <- filosofi %>%
  select(
    lcog_geo,
    ind, men, men_pauv, ind_snv,
    geom
  )

# Département
filosofi <- filosofi %>%
  mutate(
    dept = case_when(
      str_detect(lcog_geo, "^97") ~ str_sub(lcog_geo, 1, 3),
      TRUE ~ str_sub(lcog_geo, 1, 2)
    )
  )

# ======================
# 6. VALIDATION GÉOMÉTRIQUE
# ======================

message("▶ Validation des géométries")

filosofi <- st_make_valid(filosofi)
bdv      <- st_make_valid(bdv)

# ======================
# 7. SIMPLIFICATION (GAIN MAJEUR)
# ======================

message("▶ Simplification géométrique")

filosofi <- st_simplify(
  filosofi,
  dTolerance = 20,
  preserveTopology = TRUE
)

bdv <- st_simplify(
  bdv,
  dTolerance = 5,
  preserveTopology = TRUE
)

# ======================
# 8. CAST MULTIPOLYGON
# ======================

filosofi <- st_cast(filosofi, "MULTIPOLYGON")
bdv      <- st_cast(bdv, "MULTIPOLYGON")

# ======================
# 9. ID INTERNE
# ======================

filosofi$.__id__ <- seq_len(nrow(filosofi))
bdv$.__id__      <- seq_len(nrow(bdv))

# ======================
# 10. DÉCOUPAGE PAR DÉPARTEMENT
# ======================

message("▶ Découpage Filosofi")

departements <- sort(unique(filosofi$dept))

for (dep in departements) {
  
  message("   - département ", dep)
  
  x <- filter(filosofi, dept == dep)
  if (nrow(x) == 0) next
  
  st_write(
    x,
    file.path(OUT_FILOSOFI, paste0("carreaux_dep_", dep, ".gpkg")),
    delete_dsn = TRUE,
    quiet = TRUE,
    layer_options = "SPATIAL_INDEX=YES"
  )
}

message("▶ Découpage BDV")

departements_bdv <- sort(unique(bdv$codeDepartement))

for (dep in departements_bdv) {
  
  message("   - département ", dep)
  
  x <- filter(bdv, codeDepartement == dep)
  if (nrow(x) == 0) next
  
  st_write(
    x,
    file.path(OUT_BDV, paste0("bdv_dep_", dep, ".gpkg")),
    delete_dsn = TRUE,
    quiet = TRUE,
    layer_options = "SPATIAL_INDEX=YES"
  )
}

# ======================
# 11. NETTOYAGE FINAL
# ======================

rm(filosofi, bdv)
gc()

message("✅ Prétraitement terminé")


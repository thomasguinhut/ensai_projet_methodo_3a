######################
### Gestion des NA ###
######################

bv_na <- aws.s3::s3read_using(
  FUN = read_csv2,
  object = "/jointure_NA.csv",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

filosofi <- aws.s3::s3read_using(
  FUN = sf::st_read,
  object = "sources/carreaux_200m_met.gpkg",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

bv_na <- bv_na %>%
  mutate(
    x = as.numeric(gsub(".*?\\(([^,]+),.*", "\\1", geometry)),
    y = as.numeric(gsub(".*?,([^)]+).*", "\\1", geometry))
  )

bv_na <- st_as_sf(bv_na, coords = c("x", "y"), crs = 2154)

st_crs(bv_na)
st_crs(filosofi)

nearest_index <- st_nearest_feature(bv_na, filosofi)
bv_na$nearest_carreau_id <- filosofi$idcar_200m[nearest_index]
any(!is.na(bv_na$nearest_carreau_id))

bv_na_wgs84 <- bv_na %>%
  st_as_sf(coords = c("x", "y"), crs = 2154) %>%
  st_transform(., 4326)

filosofi <- st_as_sf(filosofi, sf_column_name = "geom")
filosofi_wgs84 <- st_transform(filosofi, 4326)

bv_na_wgs84 <- head(bv_na_wgs84, 10)
carreaux_associes <- unique(bv_na_wgs84$nearest_carreau_id)
filosofi_associes <- filosofi_wgs84 %>%
  filter(idcar_200m %in% carreaux_associes)

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = filosofi_associes, fill = NA, color = "blue", weight = 1, 
              popup = ~idcar_200m) %>%  # Carreaux en bleu
  addCircleMarkers(data = bv_na_wgs84, color = "red", radius = 2, stroke = FALSE, 
                   fillOpacity = 0.7, popup = ~paste("Adresse:", geo_adresse, 
                                                     "<br>Carreau affecté:", 
                                                     nearest_carreau_id))  # Adresses en rouge avec popup

# Afficher la carte
map

bv_na <- bv_na %>%
  select(-c(...1, idcar_200m, x, y))

# write.csv2(bv_na, file = "bureauvote_na_avec_id_carreau_plus_proche.csv")



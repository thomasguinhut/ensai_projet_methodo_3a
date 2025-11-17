# apres avoir execute le main
library(data.table)
library(sf)
library(ggplot2)

filo <-
  aws.s3::s3read_using(
    FUN = read.csv,
    object = "/sources/filosofi_2019_200m.csv",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )


gp <- 
  aws.s3::s3read_using(
    FUN = st_read,
    object = "/sources/carreaux_200m_met.gpkg",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )



# gp$geom[1]

gp_short <- gp %>%
  filter(lcog_geo == "2A041")

# ggplot(data = gp_short) +
#   # geom_sf dessine les polygones et utilise l'esthétique 'fill' pour la couleur
#   geom_sf(aes(fill = ind), color = NA) + # 'color = NA' enlève la bordure noire des carreaux
#   
#   # Optionnel : Ajoutez une échelle de couleurs pour la légende
#   scale_fill_viridis_c(
#     option = "magma",  # Choisissez une palette de couleurs (ex: 'viridis', 'magma', 'plasma')
#     name = "Population par carreau"
#   ) +
#   
#   # Optionnel : Améliorez le thème pour une carte
#   theme_minimal() +
#   labs(
#     title = "Répartition de la Population (Filosofi 200m)",
#     subtitle = "Visualisation par carreaux polygonaux"
#   )


adresses_df <- 
  aws.s3::s3read_using(
    FUN = read_parquet,
    object = "/sources/adresses_2022_reu.parquet",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

adresses <- st_as_sf(
  adresses_df,
  coords = c("longitude", "latitude"),  
  crs = 4326
)

carreaux <- gp %>%
  select(idcar_200m, geom)

adresses <- st_transform(adresses, st_crs(carreaux))
# glimpse(adresses, 10)

jointure <- st_join(adresses, carreaux, join = st_within)

bureau_carreau <- jointure %>%
  select(id_brut_bv_reu, idcar_200m)

# bv_par_carreau <- bureau_carreau %>%
#   group_by(idcar_200m, id_brut_bv_reu) %>% 
#   summarise(nb_adresses = n(), .groups = "drop") %>%
#   group_by(idcar_200m) %>%
#   slice_max(nb_adresses, with_ties = FALSE)
# 
# 
# bv_par_carreau <- bureau_carreau %>%
#   count(idcar_200m, id_brut_bv_reu, name = "nb_adresses") %>%   # beaucoup plus rapide
#   group_by(idcar_200m) %>%
#   slice_max(nb_adresses, with_ties = FALSE)

dt <- as.data.table(bureau_carreau)

# Compter les adresses par (carreau, bureau de vote)
counts <- dt[, .N, by = .(idcar_200m, id_brut_bv_reu)]

# Garder seulement le BV dominant par carreau
setorder(counts, idcar_200m, -N)
bv_par_carreau <- counts[, .SD[1], by = idcar_200m]

# N renvoie le nombre d'adresses pour le couple

# write.csv2(bv_par_carreau, file = "carreau_appariement_bv2.csv")


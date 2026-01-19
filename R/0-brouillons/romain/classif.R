hist(bv_2022_final$INSCRITS_T1)

bv_2022_car <- aws.s3::s3read_using(
  FUN = read_csv2,
  object = "/appariement_carreaux_filosofi/carreau_appariement_bv3.csv",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

filosofi <- aws.s3::s3read_using(
  FUN = sf::st_read,
  object = "sources/carreaux_200m_met.gpkg",
  bucket = "projet-ensai-methodo-3a", 
  opts = list("region" = "")
)

str(filosofi)
# Ind : Nombre d’individus
# Men : Nombre de ménages
# Men_pauv : Nombre de ménages pauvres
# Men_1ind : Nombre de ménages d’un seul individu
# Men_5ind : Nombre de ménages de 5 individus ou plus
# Men_prop : Nombre de ménages propriétaires
# Men_fmp : Nombre de ménages monoparentaux
# Ind_snv : Somme des niveaux de vie winsorisés des individus
# Men_surf : Somme de la surface des logements du carreau
# Men_coll : Nombre de ménages en logements collectifs
# Men_mais : Nombre de ménages en maison
# Log_av45 : Nombre de logements construits avant 1945
# Log_45_70 : Nombre de logements construits entre 1945 et 1969
# Log_70_90 : Nombre de logements construits entre 1970 et 1989
# Log_ap90 : Nombre de logements construits depuis 1990
# Log_inc : Nombre de logements dont la date de construction est inconnue
# Log_soc : Nombre de logements sociaux
# Ind_0_3 : Nombre d’individus de 0 à 3 ans
# Ind_4_5 : Nombre d’individus de 4 à 5 ans
# Ind_6_10 : Nombre d’individus de 6 à 10 ans
# Ind_11_17 : Nombre d’individus de 11 à 17 ans
# Ind_18_24 : Nombre d’individus de 18 à 24 ans
# Ind_25_39 : Nombre d’individus de 25 à 39 ans
# Ind_40_54 : Nombre d’individus de 40 à 54 ans
# Ind_55_64 : Nombre d’individus de 55 à 64 ans
# Ind_65_79 : Nombre d’individus de 65 à 79 ans
# Ind_80p : Nombre d’individus de 80 ans ou plus
# Ind_inc : Nombre d’individus dont l’âge est inconnu 

filosofi2 <- filosofi %>%
  select(-c(idcar_1km, idcar_nat, i_est_200, i_est_1km, lcog_geo))

bv_2022 <- bv_2022_final %>%
  dplyr::left_join(
    bv_2022_car,
    by = c("ID_REU" = "id_brut_bv_reu")
  )
bv_2022 <- bv_2022 %>%
  select(-c(...1))

filosofi_sans_geom <- sf::st_drop_geometry(filosofi2)

bv_2022 <- bv_2022 %>%
  dplyr::left_join(
    filosofi_sans_geom,
    by = c("idcar_200m" = "idcar_200m")
  )

str(bv_2022)

bv_agrege <- bv_2022 %>%
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

df_quant <- bv_agrege %>%
  select(where(is.numeric))
df_quant <- df_quant %>%
  select(-c(Ind, Men))

data_scaled <- scale(df_quant)

# ACP
res.acp <- PCA(data_scaled, scale.unit = TRUE, ncp = 10, graph = FALSE)

# Critère du coude
fviz_eig(res.acp, addlines = TRUE, ylim = c(0, 100))
k <- 3

res.acp$eig
coord_acp <- res.acp$ind$coord[, 1:k]

# Visualiser les bdv sur le premier plan factoriel
fviz_pca_ind(res.acp, axes = c(1, 2),
             addEllipses = TRUE,
             title = "ACP : Projection des individus")

# Visualiser le cercle des corrélations
fviz_pca_var(res.acp, axes = c(1, 2),
             col.var = "contrib",
             title = "ACP : Cercle des corrélations")
fviz_pca_var(res.acp, axes = c(1, 3),
             col.var = "contrib",
             title = "ACP : Cercle des corrélations")
fviz_pca_var(res.acp, axes = c(2, 3),
             col.var = "contrib",
             title = "ACP : Cercle des corrélations")

fviz_contrib(res.acp, choice = "var", axes = 1, top = 10)
# type de ménage (propriétaire, collectif, sociaux...) 
fviz_contrib(res.acp, choice = "var", axes = 2, top = 10)
# age
fviz_contrib(res.acp, choice = "var", axes = 3, top = 10)
# niveau de vie

inertie <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(coord_acp, centers = k, nstart = 25)
  kmeans_result$tot.withinss
})

# Tracer la courbe du coude
plot(1:10, inertie, type = "b", pch = 19,
     xlab = "Nombre de clusters (k)",
     ylab = "Inertie (variance intra-cluster)",
     main = "Méthode du coude")

set.seed(123)
kmeans_result <- kmeans(coord_acp, centers = 4, nstart = 25)

# Visualiser les clusters
fviz_cluster(kmeans_result, data = coord_acp,
             geom = "point",
             ellipse.type = "convex",
             title = "Clusters (K-means) sur les coordonnées ACP")
fviz_cluster(kmeans_result, data = coord_acp,
             geom = "point",
             ellipse.type = "convex",
             title = "Clusters (K-means) sur les coordonnées ACP",
             axes = c(1,3))


bv_agrege$Cluster <- as.factor(kmeans_result$cluster)

variables_interet <- names(df_quant)

stats_par_cluster <- bv_agrege %>%
  group_by(Cluster) %>%
  summarise(
    nb = n(),
    across(all_of(variables_interet), mean, na.rm = TRUE)
  )

print(stats_par_cluster)

results_anova <- list()
for (var in variables_interet) {
  # Effectuer le test ANOVA
  anova_result <- aov(as.formula(paste(var, "~ Cluster")), data = bv_agrege)
  
  # Récupérer le summary du test
  anova_summary <- summary(anova_result)
  
  # Extraire la statistique F et la p-value
  f_value <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  # Stocker les résultats
  results_anova[[var]] <- data.frame(
    Variable = var,
    F_value = f_value,
    P_value = p_value
  )
}

bv_2022 <- bv_2022 %>%
  left_join(bv_agrege %>% select(ID_REU, Cluster),
            by = c("ID_REU" = "ID_REU"))
bv_2022 <- bv_2022 %>%
  left_join(filosofi2 %>% select(idcar_200m, geom), 
            by = c("idcar_200m" = "idcar_200m"))
str(bv_2022)

# Exemple sur l'ille-et-Vilaine
bv_35 <- bv_2022 %>%
  filter(DEP == "35")
class(bv_35)
str(bv_35$geom)
bv_35 <- st_as_sf(bv_35)

# Representation du département avec couleur des cluster sur chaque carreaux
ggplot(data = bv_35) +
  geom_sf(aes(fill = as.factor(Cluster)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  labs(title = "Carroyages du département 35 par Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom")

st_crs(bv_35)
bv_35 <- st_transform(bv_35, 4326)

palette <- colorFactor(palette = topo.colors(5), domain = as.factor(bv_35$Cluster))
# Carte interactive
leaflet(data = bv_35) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(fillColor = ~palette(Cluster),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              popup = ~paste("Cluster:", Cluster)) %>%
  addLegend("bottomright",
            pal = palette,
            values = as.factor(bv_35$Cluster),
            title = "Cluster")

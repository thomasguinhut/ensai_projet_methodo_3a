################################################################################
############################ Importation des données ###########################
################################################################################


bv_2022_final_7 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_7.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_7)

bdd_acp <- bv_2022_final_7 %>% 
  filter(
    if_all(everything(), ~ !is.na(.x)),
    as.character(FERMETURE) == "19h",
    NX_BV == 0
  ) %>% 
  dplyr::select(
    "ID",
    starts_with(c("IND", "MEN")),
    -c("IND", "MEN", "LOG", "IND_INC"),
    ends_with("2017_T1"),
    starts_with("DENS3_P")
  ) %>%
  mutate(
    across(
      ends_with("2017_T1"),
      ~ round((.x / INSCRITS_2017_T1) * 100, 1)
    )
  ) %>% 
  dplyr::select(-c("INSCRITS_2017_T1", "VOTANTS_2017_T1")) %>%
  as.data.frame()

row.names(bdd_acp) <- bdd_acp$ID
bdd_acp$ID <- NULL
bdd_acp_scaled <- scale(bdd_acp)

# ACP
res.acp <- PCA(bdd_acp_scaled, scale.unit = FALSE, ncp = 10, graph = FALSE)

fviz_eig(res.acp, addlabels = TRUE)
fviz_pca_var(res.acp, axes = c(1,2), repel = TRUE)
fviz_pca_var(res.acp, axes = c(1,3), repel = TRUE)
# L'ACP n'a pas l'air de marcher. Il faut peut-être mieux faire une AFM.


res.mfa <- MFA(
  bdd_acp,
  group = c(19,12,3),
  type = rep("s", 3),
  ncp = 10,
  name.group = c("Filosofi", "Présidentielle 2017", "Rural/urbain"),
  graph = FALSE
)

fviz_screeplot(res.mfa, addlabels = TRUE)
fviz_mfa_axes(res.mfa,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(4, "Set2")[2:4])
fviz_mfa_axes(res.mfa,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(4, "Set2")[2:4])

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE, axes = c(1, 2))

c.mfa <- HCPC(res.MFAprop_horsMayotte, nb.clust=-1, graph = FALSE)
fviz_dend(c.mfa, k = 5, show_labels = TRUE, rect = TRUE, cex = 0.4)
fviz_cluster(c.mfa, ggtheme = theme_minimal(), repel = TRUE, labelsize = 6)




k <- 3
coord_acp <- res.acp$ind$coord[, 1:k]

set.seed(123)
kmeans_result <- kmeans(coord_acp, centers = 4, nstart = 25)

bv_2022$Cluster <- as.factor(kmeans_result$cluster)
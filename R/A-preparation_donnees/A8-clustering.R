################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

bv_2022_final_7 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_7.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_7)



################################################################################
####################### CONSTRUCTION BASE ANALYSE FACTORIELLE ##################
################################################################################


bdd_facto <- bv_2022_final_7 %>% 
  filter(
    if_all(everything(), ~ !is.na(.x)),
    as.character(FERMETURE) == "19h",
    NX_BV == 0
  ) %>% 
  dplyr::select(
    "ID",
    starts_with(c("IND", "MEN", "LOG")),
    -c("IND", "MEN", "LOG", "IND_INC"),
    ends_with("2017_T1")
  ) %>%
  mutate(
    across(
      ends_with("2017_T1"),
      ~ round((.x / INSCRITS_2017_T1) * 100, 1)
    )
  ) %>% 
  dplyr::select(-c("INSCRITS_2017_T1", "VOTANTS_2017_T1")) %>%
  as.data.frame()

row.names(bdd_facto) <- bdd_facto$ID
bdd_facto$ID <- NULL
bdd_facto_scaled <- as.data.frame(scale(bdd_facto))

glimpse(bdd_facto_scaled)



################################################################################
#################################### ACP #######################################
################################################################################


res.acp <- PCA(bdd_facto_scaled %>% dplyr::select(starts_with(c("IND", "MEN", "LOG"))),
               scale.unit = FALSE, ncp = 10, graph = FALSE)

# fviz_eig(res.acp, addlabels = TRUE)
# fviz_pca_var(res.acp, axes = c(1,2), repel = TRUE)
# fviz_pca_var(res.acp, axes = c(1,3), repel = TRUE)


set.seed(123)
c.acp <- kmeans(as.data.frame(res.acp$ind$coord[,1:4]),
                centers = 3, nstart = 25)

plot(as.data.frame(res.acp$ind$coord[,1:4]), col = c.acp$cluster,
     pch=16, cex=1.2, main="Regroupement par les k-means")


################################################################################
#################################### AFM #######################################
################################################################################


# colnames(bdd_facto)

res.mfa <- MFA(
  bdd_facto,
  group = c(20,12),
  type = rep("s", 2),
  ncp = 10,
  name.group = c("Filosofi", "Présidentielle 2017"),
  graph = FALSE
)

# res.mfa$eig
# fviz_screeplot(res.mfa, addlabels = TRUE)
# fviz_mfa_axes(res.mfa,
#               repel = TRUE,
#               axes = c(1, 2),
#               palette = brewer.pal(4, "Set2")[2:4])
# fviz_mfa_axes(res.mfa,
#               repel = TRUE,
#               axes = c(1, 3),
#               palette = brewer.pal(4, "Set2")[2:4])
# 
# fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
#              col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
# fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
#              col.var.sup = "violet", repel = TRUE, axes = c(1, 3))

set.seed(123)
c.mfa <- kmeans(as.data.frame(res.mfa$ind$coord[,1:4]),
                centers = 3, nstart = 25)

plot(as.data.frame(res.mfa$ind$coord[,1:4]), col = c.mfa$cluster,
     pch=16, cex=1.2, main="Regroupement par les k-means")

# c.mfa <- HCPC(res.mfa$ind$coord[,1:4], nb.clust=-1, graph = FALSE)
# fviz_dend(c.mfa, show_labels = FALSE)
# fviz_dend(c.mfa, k = 5, show_labels = TRUE, rect = TRUE, cex = 0.4)
# fviz_cluster(c.mfa, ggtheme = theme_minimal(), repel = TRUE, labelsize = 6)

# c.mfa$data.clust %>% 
#   tbl_summary(by = clust)



################################################################################
################################ AJOUT DES CLUSTERS ############################
################################################################################

bdd_cluster_acp <- as.data.frame(c.acp$cluster)
bdd_cluster_acp$ID <- row.names(bdd_cluster_acp)
row.names(bdd_cluster_acp) <- NULL

bdd_cluster_mfa <- as.data.frame(c.mfa$cluster)
bdd_cluster_mfa$ID <- row.names(bdd_cluster_mfa)
row.names(bdd_cluster_mfa) <- NULL

bdd_cluster <- bdd_cluster_acp %>% 
  inner_join(bdd_cluster_mfa, by = "ID") %>% 
  rename("CLUSTER_ACP" = "c.acp$cluster",
         "CLUSTER_MFA" = "c.mfa$cluster") %>% 
  dplyr::select(ID, CLUSTER_ACP, CLUSTER_MFA)

bv_2022_final_8 <- bv_2022_final_7 %>% 
  left_join(bdd_cluster, by = "ID") %>% 
  mutate(TIRABLE = !is.na(CLUSTER_ACP),
         )

glimpse(bv_2022_final_8)


################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bv_2022_final_8,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_8.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

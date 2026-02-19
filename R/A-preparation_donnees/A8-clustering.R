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
    NX_BV == 0
  ) %>% 
  dplyr::select(
    "ID",
    starts_with("DENS3_P"),
    starts_with(c("PROP_IND", "PROP_MEN", "PROP_LOG", "MOY")),
    -c("IND", "MEN", "LOG"),
    starts_with("PROP") & ends_with(c("2017_T1", "2017_T2"))
  ) %>% 
  as.data.frame()

row.names(bdd_facto) <- bdd_facto$ID
bdd_facto$ID <- NULL

glimpse(bdd_facto)



################################################################################
########################### AFM - grille densité x Filosofi ####################
################################################################################


colnames(bdd_facto)

res.mfa_densite_filosofi <- MFA(
  bdd_facto %>% dplyr::select(starts_with(c("DENS3_P", "PROP_IND",
                                            "PROP_MEN", "PROP_LOG", "MOY"))),
  group = c(3, 20),
  type = rep("s", 2),
  ncp = 10,
  name.group = c("Grille densité", "Filosofi"),
  graph = FALSE
)

plot_variances_dimensions(res.mfa_densite_filosofi)

fviz_mfa_axes(res.mfa_densite_filosofi,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(5, "Set2")[2:5])
fviz_mfa_axes(res.mfa_densite_filosofi,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(5, "Set2")[2:5])

fviz_mfa_var(res.mfa_densite_filosofi, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
fviz_mfa_var(res.mfa_densite_filosofi, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 3))

coords.mfa_densite_filosofi <- res.mfa_densite_filosofi$ind$coord[, 1:6]
md_mfa_densite_filosofi <- dist(coords.mfa_densite_filosofi)
c.mfa_densite_filosofi <- fastcluster::hclust(md_mfa_densite_filosofi,
                                              method = "ward.D2")
plot(sort(c.mfa_densite_filosofi$height, decreasing = TRUE)[1:20], type = "s",
     xlab = "Nombre de classes", ylab = "Inertie")



################################################################################
###################### AFM - grille densité x Filosofi x 2017 ##################
################################################################################


colnames(bdd_facto)

res.mfa_densite_filosofi_2017 <- MFA(
  bdd_facto,
  group = c(3, 20, 12, 3),
  type = rep("s", 4),
  ncp = 15,
  name.group = c("Grille densité", "Filosofi", "Présidentielle 2017 T1",
                 "Présidentielle 2017 T2"),
  graph = FALSE
)

plot_variances_dimensions(res.mfa_densite_filosofi_2017)

fviz_mfa_axes(res.mfa_densite_filosofi_2017,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(5, "Set2")[2:5])
fviz_mfa_axes(res.mfa_densite_filosofi_2017,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(5, "Set2")[2:5])

fviz_mfa_var(res.mfa_densite_filosofi_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
fviz_mfa_var(res.mfa_densite_filosofi_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 3))

coords.mfa_densite_filosofi_2017 <- (
  res.mfa_densite_filosofi_2017$ind$coord[, 1:11]
)
md_mfa_densite_filosofi_2017 <- dist(coords.mfa_densite_filosofi_2017)
c.mfa_densite_filosofi_2017 <- fastcluster::hclust(md_mfa_densite_filosofi_2017,
                                                   method = "ward.D2")
plot(sort(c.mfa_densite_filosofi_2017$height, decreasing = TRUE)[1:20],
     type = "s", xlab = "Nombre de classes", ylab = "Inertie")



################################################################################
############################ AFM - grille densité x 2017 #######################
################################################################################


colnames(bdd_facto)

res.mfa_densite_2017 <- MFA(
  bdd_facto %>% dplyr::select(starts_with("DENS3_P"), ends_with(c("2017_T1",
                                                                  "2017_T2"))),
  group = c(3, 12, 3),
  type = rep("s", 3),
  ncp = 10,
  name.group = c("Grille densité", "Présidentielle 2017 T1",
                 "Présidentielle 2017 T2"),
  graph = FALSE
)

plot_variances_dimensions(res.mfa_densite_2017)

fviz_mfa_axes(res.mfa_densite_2017,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(5, "Set2")[2:5])
fviz_mfa_axes(res.mfa_densite_2017,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(5, "Set2")[2:5])

fviz_mfa_var(res.mfa_densite_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
fviz_mfa_var(res.mfa_densite_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 3))

coords.mfa_densite_20147 <- res.mfa_densite_2017$ind$coord[, 1:8]
md_mfa_densite_2017 <- dist(coords.mfa_densite_20147)
c.mfa_densite_2017 <- fastcluster::hclust(md_mfa_densite_2017,
                                              method = "ward.D2")
plot(sort(c.mfa_densite_2017$height, decreasing = TRUE)[1:20], type = "s",
     xlab = "Nombre de classes", ylab = "Inertie")



################################################################################
################################ AJOUT DES CLUSTERS ############################
################################################################################


bdd_cluster_afm_densite_filosofi <- data.frame(
  CLUSTER_AFM_DENSITE_FILOSOFI_6 = cutree(c.mfa_densite_filosofi, 6))

bdd_cluster_afm_densite_filosofi$CLUSTER_AFM_DENSITE_FILOSOFI_6 <- as.character(
  bdd_cluster_afm_densite_filosofi$CLUSTER_AFM_DENSITE_FILOSOFI_6
)

bdd_cluster_afm_densite_filosofi$ID <- row.names(
  bdd_cluster_afm_densite_filosofi)
row.names(bdd_cluster_afm_densite_filosofi) <- NULL



bdd_cluster_afm_densite_filosofi_2017 <- data.frame(
  CLUSTER_AFM_DENSITE_FILOSOFI_2017_8 = cutree(c.mfa_densite_filosofi_2017, 8))

bdd_cluster_afm_densite_filosofi_2017$CLUSTER_AFM_DENSITE_FILOSOFI_2017_8 <- (
  as.character(
    bdd_cluster_afm_densite_filosofi_2017$CLUSTER_AFM_DENSITE_FILOSOFI_2017_8
  ))

bdd_cluster_afm_densite_filosofi_2017$ID <- row.names(
  bdd_cluster_afm_densite_filosofi_2017)
row.names(bdd_cluster_afm_densite_filosofi_2017) <- NULL



bdd_cluster_afm_densite_2017 <- data.frame(
  CLUSTER_AFM_DENSITE_2017_7 = cutree(c.mfa_densite_2017, 7))

bdd_cluster_afm_densite_2017$CLUSTER_AFM_DENSITE_2017_7 <- (
  as.character(
    bdd_cluster_afm_densite_2017$CLUSTER_AFM_DENSITE_2017_7
  ))

bdd_cluster_afm_densite_2017$ID <- row.names(
  bdd_cluster_afm_densite_2017)
row.names(bdd_cluster_afm_densite_2017) <- NULL



bdd_cluster <- bdd_cluster_afm_densite_filosofi %>% 
  inner_join(bdd_cluster_afm_densite_filosofi_2017, by = "ID") %>% 
  dplyr::select(ID,
                CLUSTER_AFM_DENSITE_FILOSOFI_6,
                CLUSTER_AFM_DENSITE_FILOSOFI_2017_8) %>% 
  inner_join(bdd_cluster_afm_densite_2017, by = "ID") %>% 
  dplyr::select(ID,
                CLUSTER_AFM_DENSITE_FILOSOFI_6,
                CLUSTER_AFM_DENSITE_FILOSOFI_2017_8,
                CLUSTER_AFM_DENSITE_2017_7)

bv_2022_final_8 <- bv_2022_final_7 %>% 
  left_join(bdd_cluster, by = "ID") %>% 
  mutate(TIRABLE = ifelse(!NX_BV & INSCRITS_T1 > 99 & FERMETURE == "19h",
                          TRUE, FALSE))

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

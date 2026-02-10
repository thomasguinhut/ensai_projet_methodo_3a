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
    starts_with("DENS3_P"),
    starts_with(c("IND", "MEN", "LOG")),
    -c("IND", "MEN", "LOG", "IND_INC"),
    starts_with("PROP") & ends_with("2017_T1")
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
  bdd_facto %>% dplyr::select(starts_with(c("DENS3_P", "IND", "MEN", "LOG"))),
  group = c(3, 20),
  type = rep("s", 2),
  ncp = 10,
  name.group = c("Grille densité", "Filosofi"),
  graph = FALSE
)

res.mfa_densite_filosofi$eig %>%
  as.data.frame() %>%
  mutate(
    PC = row_number(),
    var = `percentage of variance`,
    cum = `cumulative percentage of variance`,
    cum_scaled = cum / max(cum) * max(var)
  ) %>%
  ggplot(aes(x = PC)) +
  geom_col(aes(y = var),
           fill = "steelblue") +
  geom_text(
    aes(y = var, label = round(var, 0)),
    vjust = -0.5,
    color = "steelblue",
    size = 3
  ) +
  geom_line(aes(y = cum_scaled),
            color = "red") +
  geom_point(aes(y = cum_scaled),
             color = "red") +
  geom_text(
    aes(y = cum_scaled, label = round(cum, 0)),
    vjust = -0.8,
    color = "red",
    size = 3
  ) +
  scale_y_continuous(
    name = "Percentage of variances\n",
    sec.axis = sec_axis(
      ~ . * max(res.mfa_densite_filosofi$eig[,3]) / max(res.mfa_densite_filosofi$eig[,2]),
      name = "Cumulative percentage of variances\n"
    )
  ) +
  labs(
    title = "Variances explained\n",
    x = "\nPrincipal Components"
  ) +
  theme_minimal()

fviz_mfa_axes(res.mfa_densite_filosofi,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(4, "Set2")[2:4])
fviz_mfa_axes(res.mfa_densite_filosofi,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(4, "Set2")[2:4])

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
  group = c(3, 20, 12),
  type = rep("s", 3),
  ncp = 15,
  name.group = c("Grille densité", "Filosofi", "Présidentielle 2017"),
  graph = FALSE
)

res.mfa_densite_filosofi_2017$eig %>%
  as.data.frame() %>%
  mutate(
    PC = row_number(),
    var = `percentage of variance`,
    cum = `cumulative percentage of variance`,
    cum_scaled = cum / max(cum) * max(var)
  ) %>%
  ggplot(aes(x = PC)) +
  geom_col(aes(y = var),
           fill = "steelblue") +
  geom_text(
    aes(y = var, label = round(var, 0)),
    vjust = -0.5,
    color = "steelblue",
    size = 3
  ) +
  geom_line(aes(y = cum_scaled),
            color = "red") +
  geom_point(aes(y = cum_scaled),
             color = "red") +
  geom_text(
    aes(y = cum_scaled, label = round(cum, 0)),
    vjust = -0.8,
    color = "red",
    size = 3
  ) +
  scale_y_continuous(
    name = "Percentage of variances\n",
    sec.axis = sec_axis(
      ~ . * max(res.mfa_densite_filosofi_2017$eig[,3]) / max(res.mfa_densite_filosofi_2017$eig[,2]),
      name = "Cumulative percentage of variances\n"
    )
  ) +
  labs(
    title = "Variances explained\n",
    x = "\nPrincipal Components"
  ) +
  theme_minimal()

fviz_mfa_axes(res.mfa_densite_filosofi_2017,
              repel = TRUE,
              axes = c(1, 2),
              palette = brewer.pal(4, "Set2")[2:4])
fviz_mfa_axes(res.mfa_densite_filosofi_2017,
              repel = TRUE,
              axes = c(1, 3),
              palette = brewer.pal(4, "Set2")[2:4])

fviz_mfa_var(res.mfa_densite_filosofi_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
fviz_mfa_var(res.mfa_densite_filosofi_2017, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, axes = c(1, 3))

coords.mfa_densite_filosofi_2017 <- res.mfa_densite_filosofi_2017$ind$coord[, 1:12]
md_mfa_densite_filosofi_2017 <- dist(coords.mfa_densite_filosofi_2017)
c.mfa_densite_filosofi_2017 <- fastcluster::hclust(md_mfa_densite_filosofi_2017,
                                              method = "ward.D2")
plot(sort(c.mfa_densite_filosofi_2017$height, decreasing = TRUE)[1:20],
     type = "s", xlab = "Nombre de classes", ylab = "Inertie")



################################################################################
################################ AJOUT DES CLUSTERS ############################
################################################################################


bdd_cluster_afm_densite_filosofi <- data.frame(
  CLUSTER_AFM_DENSITE_FILOSOFI = cutree(c.mfa_densite_filosofi, 5))
bdd_cluster_afm_densite_filosofi$ID <- row.names(
  bdd_cluster_afm_densite_filosofi)
row.names(bdd_cluster_afm_densite_filosofi) <- NULL

bdd_cluster_afm_densite_filosofi_2017 <- data.frame(
  CLUSTER_AFM_DENSITE_FILOSOFI_2017 = cutree(c.mfa_densite_filosofi_2017, 5))
bdd_cluster_afm_densite_filosofi_2017$ID <- row.names(
  bdd_cluster_afm_densite_filosofi_2017)
row.names(bdd_cluster_afm_densite_filosofi_2017) <- NULL

bdd_cluster <- bdd_cluster_afm_densite_filosofi %>% 
  inner_join(bdd_cluster_afm_densite_filosofi_2017, by = "ID") %>% 
  dplyr::select(ID, CLUSTER_AFM_DENSITE_FILOSOFI,
                CLUSTER_AFM_DENSITE_FILOSOFI_2017)

bv_2022_final_8 <- bv_2022_final_7 %>% 
  left_join(bdd_cluster, by = "ID") %>% 
  mutate(TIRABLE = !is.na(CLUSTER_AFM_DENSITE_FILOSOFI),
         )

colSums(is.na(bv_2022_final_8))

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

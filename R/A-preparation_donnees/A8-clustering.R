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


df_quant <- bv_2022 %>%
  dplyr::select(where(is.numeric))
df_quant <- df_quant %>%
  select(-c(Ind, Men))

data_scaled <- scale(df_quant)

# ACP
res.acp <- PCA(data_scaled, scale.unit = TRUE, ncp = 10, graph = FALSE)

k <- 3
coord_acp <- res.acp$ind$coord[, 1:k]

set.seed(123)
kmeans_result <- kmeans(coord_acp, centers = 4, nstart = 25)

bv_2022$Cluster <- as.factor(kmeans_result$cluster)
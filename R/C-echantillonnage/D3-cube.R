n <- 500
N <- nrow(base_sondage)

x <- base_sondage %>% 
  dplyr::select(
    "ID",
    ends_with("2017_T1"),
    "DENS3",
    starts_with(c("IND", "MEN", "LOG")), 
    -c("IND", "MEN", "LOG"),
    -starts_with("PROP")
  ) %>% 
  mutate(
    RURAL = ifelse(DENS3 == "3", 1, 0),
    URBAIN_INTERM = ifelse(DENS3 == "2", 1, 0),
    URBAIN_DENSE = ifelse(DENS3 == "1", 1, 0)
  ) %>% 
  dplyr::select(INSCRITS_2017_T1, EXPRIMES_2017_T1, MACRON_2017_T1,
                LEPEN_2017_T1, FILLON_2017_T1, MELENCHON_2017_T1, HAMON_2017_T1,
                DUPONTAIGNAN_2017_T1, LASSALLE_2017_T1, POUTOU_2017_T1,
                ASSELINEAU_2017_T1, ARTHAUD_2017_T1, CHEMINADE_2017_T1,
                RURAL, URBAIN_INTERM, URBAIN_DENSE, starts_with("MEN"), "LOG_SOC",
                rev(starts_with("IND")), -DENS3, -VOTANTS_2017_T1) %>% 
  as.data.frame()

row.names(x) <- x$ID
x$ID <- NULL

colnames(x)

# res.mfa_x <- MFA(
#   x,
#   group = c(18, 14, 3),
#   type = rep("s", 3),
#   ncp = 15,
#   name.group = c("Filosofi", "Présidentielle 2017", "Grille densité"),
#   graph = FALSE
# )
# 
# plot_variances_dimensions(res.pca_x)
# plot(res.pca_x)
# fviz_mfa_var(res.mfa_x, "quanti.var", palette = "jco",
#              col.var.sup = "violet", repel = TRUE, axes = c(1, 2))
# 
# coords.mfa_x <- res.pca_x$ind$coord[, 1]

PI <- rep(n/N, N)

bdd_cube <- cbind(PI, as.matrix(x))

# corrplot::corrplot(cor(bdd_cube[,-1], use = "complete.obs"), method = "color",
#                    type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.65)

ech <- samplecube(bdd_cube, PI, method = 2)

ech_cube <- tirage_bulletins(base_sondage, ech, "T1", "cube", 100)


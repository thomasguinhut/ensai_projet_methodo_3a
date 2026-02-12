# Fonction pour analyser et visualiser les variances et boxplots
analyse_boxplot <- function(variables) {
  # Calcul des variances et tri par ordre décroissant
  variances <- sapply(bv_2022_final[variables], var, na.rm = TRUE)
  variances_triees <- sort(variances, decreasing = TRUE)
  print(variances_triees)
  
  # Préparation des données pour le boxplot (centrage des données)
  data_long <- bv_2022_final %>%
    select(all_of(variables)) %>%
    pivot_longer(cols = everything(), names_to = "Candidat", values_to = "Proportion") %>%
    na.omit() %>%
    group_by(Candidat) %>%
    mutate(Proportion_centree = Proportion - mean(Proportion, na.rm = TRUE))
  
  # Tri des candidats selon l'ordre des variances décroissantes
  data_long$Candidat <- factor(data_long$Candidat, levels = names(variances_triees))
  
  # Génération du boxplot sans légende
  p <- ggplot(data_long, aes(x = Candidat, y = Proportion_centree, fill = Candidat)) +
    geom_boxplot() +
    labs(title = paste("Boxplot des proportions centrées (", deparse(substitute(variables)), ")", sep = ""),
         x = "Candidat",
         y = "Proportion centrée") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  return(p)
}

# Première application : variables 2017
variables_2017 <- c(
  "PROP_DUPONTAIGNAN_2017_T1",
  "PROP_LEPEN_2017_T1",
  "PROP_MACRON_2017_T1",
  "PROP_HAMON_2017_T1",
  "PROP_ARTHAUD_2017_T1",
  "PROP_POUTOU_2017_T1",
  "PROP_CHEMINADE_2017_T1",
  "PROP_LASSALLE_2017_T1",
  "PROP_MELENCHON_2017_T1",
  "PROP_ASSELINEAU_2017_T1",
  "PROP_FILLON_2017_T1"
)
boxplot_2017 <- analyse_boxplot(variables_2017)
print(boxplot_2017)

# Deuxième application : nouvelles variables T1
variables_T1 <- c(
  "ARTHAUD_T1", "ROUSSEL_T1", "MACRON_T1", "LASSALLE_T1", "LEPEN_T1",
  "ZEMMOUR_T1", "MELENCHON_T1", "HIDALGO_T1", "JADOT_T1", "PECRESSE_T1",
  "POUTOU_T1", "DUPONTAIGNAN_T1"
)
boxplot_T1 <- analyse_boxplot(variables_T1)
print(boxplot_T1)

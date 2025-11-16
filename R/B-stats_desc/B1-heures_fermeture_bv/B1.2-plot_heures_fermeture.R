# 1. Préparation des données par région
df_region <- bv_2022_final %>%
  group_by(REG_LIB, FERMETURE) %>%
  summarise(nb_bv = n(), .groups = "drop") %>%
  group_by(REG_LIB) %>%
  mutate(prop = nb_bv / sum(nb_bv)) %>%
  ungroup()

# 2. Ajout de la ligne France
df_france <- bv_2022_final %>%
  group_by(FERMETURE) %>%
  summarise(nb_bv = n(), .groups = "drop") %>%
  mutate(REG_LIB = "France", prop = nb_bv / sum(nb_bv))

# Fusion des données régionales + France
df_plot <- bind_rows(df_region, df_france) %>%
  # Définir l'ordre dans les barres (superposition)
  mutate(FERMETURE = factor(FERMETURE, levels = c("20h", "19h"))) %>%
  # Calcul de la proportion de 20h pour le tri
  group_by(REG_LIB) %>%
  mutate(prop_20h = sum(prop[FERMETURE == "20h"], na.rm = TRUE)) %>%
  ungroup()

# 3. Réordonner les régions
df_plot_ordered <- df_plot %>%
  mutate(REG_LIB = fct_reorder(REG_LIB, prop_20h)) %>%   # tri du plus petit au plus grand
  mutate(REG_LIB = fct_relevel(REG_LIB, "France", after = 0))  # France tout en bas

levels(df_plot_ordered$REG_LIB)

# 4. Graphique
p <- ggplot(df_plot_ordered, aes(x = REG_LIB, y = prop, fill = FERMETURE)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("19h" = "#1b9e77", "20h" = "#d95f02"),
    breaks = c("19h", "20h"),
    name = NULL
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    plot.caption = element_blank()
  )

# 5. Exportation du graphique en PDF A4 portrait
ggsave("R/B-stats_desc/B1-heures_fermeture_bv/fermeture_bv_par_region.pdf", plot = p, width = 8.27, height = 11.69, units = "in")
# system(paste0("mc cp ", nom_fichier_D, " ", nom_fichier_aws), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

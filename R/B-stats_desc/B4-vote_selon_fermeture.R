bdd_analyse <- bv_2022_final %>% 
  dplyr::mutate(
    dplyr::across(
      c(MACRON_T1, LEPEN_T1, MELENCHON_T1, ZEMMOUR_T1,
        HIDALGO_T1, JADOT_T1, PECRESSE_T1, POUTOU_T1,
        DUPONTAIGNAN_T1, ROUSSEL_T1, ARTHAUD_T1, LASSALLE_T1),
      ~ .x / INSCRITS_T1,
      .names = "PROP_{.col}"
    )
  ) %>% 
  dplyr::select(PROP_MACRON_T1, PROP_LEPEN_T1, PROP_MELENCHON_T1, FERMETURE) %>% 
  tidyr::pivot_longer(cols = -FERMETURE)


ggplot(bdd_analyse, aes(x=name, y=value, fill=FERMETURE)) + 
  geom_boxplot()

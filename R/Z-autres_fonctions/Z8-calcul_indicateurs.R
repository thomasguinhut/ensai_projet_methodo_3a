# Valeurs réelles
valeurs_reelles <- c(MACRON = 27.84, LEPEN = 23.15, MELENCHON = 21.95)

# Fonction pour calculer les indicateurs
calcul_indicateurs <- function(data, valeurs_reelles) {
  data %>%
    group_by(methode, candidat) %>%
    summarise(
      biais = mean(estimation - valeurs_reelles[candidat]),
      variance = var(estimation),
      rrmse = sqrt(mean((estimation - valeurs_reelles[candidat])^2)) / valeurs_reelles[candidat],
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = candidat, values_from = c(biais, variance, rrmse),
                names_sep = "_")
}

# Fonction pour calculer les taux de réussite du classement
calcul_taux_classement <- function(data) {
  data_classement <- data %>%
    group_by(simulation, methode) %>%
    mutate(
      macron_premier = estimation[candidat == "MACRON"] > estimation[candidat == "LEPEN"] &
        estimation[candidat == "MACRON"] > estimation[candidat == "MELENCHON"],
      lepen_deuxieme = estimation[candidat == "LEPEN"] > estimation[candidat == "MELENCHON"] &
        estimation[candidat == "LEPEN"] < estimation[candidat == "MACRON"],
      melenchon_troisieme = estimation[candidat == "MELENCHON"] < estimation[candidat == "LEPEN"] &
        estimation[candidat == "MELENCHON"] < estimation[candidat == "MACRON"]
    ) %>%
    distinct(simulation, methode, .keep_all = TRUE) %>%
    group_by(methode) %>%
    summarise(
      taux_macron_premier = mean(macron_premier, na.rm = TRUE),
      taux_lepen_deuxieme = mean(lepen_deuxieme, na.rm = TRUE),
      taux_melenchon_troisieme = mean(melenchon_troisieme, na.rm = TRUE),
      .groups = "drop"
    )
  return(data_classement)
}

# Fonction pour calculer les écarts moyens
calcul_ecarts_moyens <- function(data) {
  data_ecarts <- data %>%
    group_by(simulation, methode) %>%
    summarise(
      écart_macron_lepen = estimation[candidat == "MACRON"] - estimation[candidat == "LEPEN"],
      écart_lepen_melenchon = estimation[candidat == "LEPEN"] - estimation[candidat == "MELENCHON"],
      .groups = "drop"
    ) %>%
    group_by(methode) %>%
    summarise(
      écart_macron_lepen_moyen = mean(écart_macron_lepen, na.rm = TRUE),
      écart_lepen_melenchon_moyen = mean(écart_lepen_melenchon, na.rm = TRUE),
      .groups = "drop"
    )
  return(data_ecarts)
}

# Calcul des indicateurs pour 300 et 600
indicateurs_300 <- calcul_indicateurs(rds300, valeurs_reelles)
indicateurs_600 <- calcul_indicateurs(rds600, valeurs_reelles)

taux_classement_300 <- calcul_taux_classement(rds300)
taux_classement_600 <- calcul_taux_classement(rds600)

ecarts_moyens_300 <- calcul_ecarts_moyens(rds300)
ecarts_moyens_600 <- calcul_ecarts_moyens(rds600)

# Fusion des résultats pour 300
resultats_300 <- full_join(indicateurs_300, taux_classement_300, by = "methode") %>%
  full_join(ecarts_moyens_300, by = "methode") %>%
  mutate(taille_echantillon = 300)

# Fusion des résultats pour 600
resultats_600 <- full_join(indicateurs_600, taux_classement_600, by = "methode") %>%
  full_join(ecarts_moyens_600, by = "methode") %>%
  mutate(taille_echantillon = 600)

# Combinaison des résultats
resultats_combines <- bind_rows(resultats_300, résultats_600)

# Affichage des résultats
print(resultats_combines)
bdd <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    # Mettre les options de FUN ici
    delim = ",",
    object = "projet_methodo_3a/communes_2022.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

str(bdd)

# Dictionnaire pour convertir les codes REG en noms de régions (y compris DROM)
regions <- data.frame(
  REG = c("01", "02", "03", "04", "06", "11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94"),
  NOM_REG = c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte",
              "Île-de-France", "Centre-Val de Loire", "Bourgogne-Franche-Comté", "Normandie",
              "Hauts-de-France", "Grand Est", "Pays de la Loire", "Bretagne",
              "Île-de-France", "Occitanie", "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Corse"),
  stringsAsFactors = FALSE
)

# Conversion des codes REG en noms de régions
bdd_1 <- bdd %>%
  filter(TYPECOM == "COM") %>%
  dplyr::select(COM, REG, DEP, LIBELLE) %>%
  left_join(regions, by = "REG") %>%
  rename(NOM_REGION = NOM_REG) %>%
  dplyr::select(-REG)

# Ajout des variables pour les horaires et dates d'ouverture/fermeture des bureaux de vote (2 tours)
bdd_2 <- bdd_1 %>%
  mutate(
    heure_ouverture_bv = case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "07:00",  # Heure d'ouverture anticipée pour les DROM
      TRUE ~ "08:00"  # Heure par défaut pour la métropole
    ),
    heure_fermeture_bv = case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "17:00",  # Fermeture anticipée pour les DROM
      TRUE ~ "18:00"  # Heure par défaut pour la métropole
    ),
    date_ouverture_tour1 = as.Date(case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "2022-04-09",  # 1er tour (samedi pour les DROM)
      TRUE ~ "2022-04-10"  # 1er tour (dimanche pour la métropole)
    )),
    date_fermeture_tour1 = as.Date(case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "2022-04-09",  # 1er tour (samedi pour les DROM)
      TRUE ~ "2022-04-10"  # 1er tour (dimanche pour la métropole)
    )),
    date_ouverture_tour2 = as.Date(case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "2022-04-23",  # 2nd tour (samedi pour les DROM)
      TRUE ~ "2022-04-24"  # 2nd tour (dimanche pour la métropole)
    )),
    date_fermeture_tour2 = as.Date(case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "2022-04-23",  # 2nd tour (samedi pour les DROM)
      TRUE ~ "2022-04-24"  # 2nd tour (dimanche pour la métropole)
    ))
  )
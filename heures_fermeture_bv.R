bdd <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    # Mettre les options de FUN ici
    delim = ",",
    object = "diffusion/projet_methodo_3a/communes_2022.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(bdd)

# Dictionnaire pour convertir les codes REG en noms de régions (y compris DROM)
regions <- data.frame(
  REG = c("01", "02", "03", "04", "06", "11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94"),
  NOM_REG = c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte",
              "Île-de-France", "Centre-Val de Loire", "Bourgogne-Franche-Comté", "Normandie",
              "Hauts-de-France", "Grand Est", "Pays de la Loire", "Bretagne",
              "Nouvelle-Aquitaine", "Occitanie", "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Corse"),
  stringsAsFactors = FALSE
)

# Conversion des codes REG en noms de régions
bdd_1 <- bdd %>%
  filter(TYPECOM == "COM") %>%
  dplyr::select(COM, REG, DEP, LIBELLE) %>%
  left_join(regions, by = "REG") %>%
  rename(NOM_REGION = NOM_REG) %>%
  dplyr::select(-REG)

glimpse(bdd_1)

################################################################################
######################## Liste des communes fermant à 20h ######################
################################################################################

paca20 <- c(
  "04070", # Digne-les-Bains
  "05061", # Gap
  "06004", "06006", "06007", "06011", "06027", "06029", "06030", "06032", "06033", "06039", "06046", "06059", "06060", "06069", "06079", "06083", "06084", "06085", "06088", "06090", "06091", "06104", "06105", "06108", "06114", "06121", "06123", "06138", "06149", "06152", "06155", "06157", "06159", "06161",
  "13055", # Marseille
  "84007", #Avignon
)

na20 <- "33063" # Bordeaux

norm20 <- "14118" # Caen

aura20 <- c(
  "38185", # Grenoble
  "42218", # Saint-Étienne
  "69123", "69266", # Lyon et Villeurbanne
  "73065" # Chambéry
)

pdll20 <- c("44109", "44162") # Nantes et Saint-Herblain

ge20 <- c(
  "54395", # Nancy
  "57463", # Metz
  "67482" # Strasbourg
)

occ20 <- c(
  "31044", "31069", "31113", "31116", "31149", "31157", "31187", "31395", "31424", "31446", "31483", "31488", "31506", "31555", "31557", "31561", "31588",
  "34172" # Montpallier
)

idf20 <- bdd_1$COM[grepl("^(75|78|91|92|93|94|95)", bdd_1$COM)] # Toutes sauf celles de Seine-et-Marne

# Ajout des variables pour les horaires et dates d'ouverture/fermeture des bureaux de vote (2 tours)
bdd_2 <- bdd_1 %>%
  mutate(
    heure_ouverture_bv = case_when(
      DEP %in% c("971", "972", "973", "974", "976") ~ "07:00",  # Heure d'ouverture anticipée pour les DROM
      TRUE ~ "08:00"  # Heure par défaut pour la métropole
    ),
    heure_fermeture_bv = case_when(
      NOM_REGION == "Nouvelle-Aquitaine" & !(COM %in% na20) ~ "19:00",
      NOM_REGION == "Provence-Alpes-Côte d'Azur" & !(COM %in% paca20) ~ "19:00",
      COM %in% c(na20, paca20) ~ "20:00"
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

sum(is.na(bdd_2$heure_fermeture_bv))

    
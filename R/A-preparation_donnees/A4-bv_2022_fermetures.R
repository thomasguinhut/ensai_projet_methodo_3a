################################################################################
############################ Importation des données ###########################
################################################################################

communes_2022_1 <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "/sources/communes_2022.csv",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = ""),
    show_col_types = FALSE
  )

glimpse(communes_2022_1)

bv_2022_final_3 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_3.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_3)


################################################################################
################ Ajout noms départements et régions de métropole ###############
################################################################################

regions <- data.frame(
  REG = c("11", "24", "27",
          "28", "32", "44", "52",
          "53", "75", "76",
          "84", "93", "94"),
  REG_LIB = c("Île-de-France", "Centre-Val de Loire", "Bourgogne-Franche-Comté",
              "Normandie", "Hauts-de-France", "Grand Est", "Pays de la Loire",
              "Bretagne", "Nouvelle-Aquitaine", "Occitanie",
              "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Corse")
)

regions$REG_LIB <- factor(regions$REG_LIB)

glimpse(regions)

departements <- data.frame(
  DEP = c(
    "01", "02", "03", "04", "05",
    "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "21",
    "22", "23", "24", "25", "26",
    "27", "28", "29", "2A", "2B",
    "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44",
    "45", "46", "47", "48", "49",
    "50", "51", "52", "53", "54",
    "55", "56", "57", "58", "59",
    "60", "61", "62", "63", "64",
    "65", "66", "67", "68", "69",
    "70", "71", "72", "73", "74",
    "75", "76", "77", "78", "79",
    "80", "81", "82", "83", "84",
    "85", "86", "87", "88", "89",
    "90", "91", "92", "93",
    "94", "95"
  ),
  DEP_LIB = c(
    "Ain", "Aisne", "Allier", "Alpes-de-Haute-Provence", "Hautes-Alpes",
    "Alpes-Maritimes", "Ardèche", "Ardennes", "Ariège", "Aube",
    "Aude", "Aveyron", "Bouches-du-Rhône", "Calvados", "Cantal",
    "Charente", "Charente-Maritime", "Cher", "Corrèze", "Côte-d'Or",
    "Côtes-d'Armor", "Creuse", "Dordogne", "Doubs", "Drôme",
    "Eure", "Eure-et-Loir", "Finistère", "Corse-du-Sud", "Haute-Corse",
    "Gard", "Haute-Garonne", "Gers", "Gironde", "Hérault",
    "Ille-et-Vilaine", "Indre", "Indre-et-Loire", "Isère", "Jura",
    "Landes", "Loir-et-Cher", "Loire", "Haute-Loire", "Loire-Atlantique",
    "Loiret", "Lot", "Lot-et-Garonne", "Lozère", "Maine-et-Loire",
    "Manche", "Marne", "Haute-Marne", "Mayenne", "Meurthe-et-Moselle",
    "Meuse", "Morbihan", "Moselle", "Nièvre", "Nord",
    "Oise", "Orne", "Pas-de-Calais", "Puy-de-Dôme", "Pyrénées-Atlantiques",
    "Hautes-Pyrénées", "Pyrénées-Orientales", "Bas-Rhin", "Haut-Rhin", "Rhône",
    "Haute-Saône", "Saône-et-Loire", "Sarthe", "Savoie", "Haute-Savoie",
    "Paris", "Seine-Maritime", "Seine-et-Marne", "Yvelines", "Deux-Sèvres",
    "Somme", "Tarn", "Tarn-et-Garonne", "Var", "Vaucluse",
    "Vendée", "Vienne", "Haute-Vienne", "Vosges", "Yonne",
    "Territoire de Belfort", "Essonne", "Hauts-de-Seine", "Seine-Saint-Denis",
    "Val-de-Marne", "Val-d'Oise"
  ),
  stringsAsFactors = FALSE
)

departements$DEP_LIB <- factor(departements$DEP_LIB)

glimpse(departements)

# On ajoute les noms à la base et on retire les Outre-mer
communes_2022_2 <- communes_2022_1 %>%
  rename(COM_LIB = LIBELLE) %>% 
  filter(TYPECOM == "COM") %>%
  dplyr::select(COM, REG, DEP, COM_LIB) %>%
  left_join(regions, by = "REG") %>%
  left_join(departements, by = "DEP") %>% 
  filter(!(REG %in%c("01", "02", "03", "04", "06")))

glimpse(communes_2022_2)

################################################################################
######################## Liste des communes fermant à 20h ######################
################################################################################

paca_20h <- c(
  "04070", # Digne-les-Bains
  "05061", # Gap
  "06004", "06006", "06007", "06011", "06027", "06029", "06030", "06032",
      "06033", "06039", "06046", "06059", "06060", "06069", "06079", "06083",
      "06084", "06085", "06088", "06090", "06091", "06104", "06105", "06108",
      "06114", "06121", "06123", "06138", "06149", "06152", "06155", "06157",
      "06159", "06161", # 34 communes des Alpes-maritimes
  "13055", # Marseille
  "84007"  #Avignon
)

na_20h <- "33063" # Bordeaux

norm_20h <- "14118" # Caen

aura_20h <- c(
  "38185", # Grenoble
  "42218", # Saint-Étienne
  "69123", "69266", # Lyon et Villeurbanne
  "73065" # Chambéry
)

pdll_20h <- c("44109", "44162") # Nantes et Saint-Herblain

ge_20h <- c(
  "54395", # Nancy
  "57463", # Metz
  "67482" # Strasbourg
)

occ_20h <- c(
  "31044", "31069", "31113", "31116", "31149", "31157", "31187", "31395",
      "31424", "31446", "31483", "31488", "31506", "31555", "31557", "31561",
      "31588", # 17 communes de la Haute-Garonne
  "34172" # Montpallier
)

idf_20h <- communes_2022_2$COM[
  grepl("^(75|78|91|92|93|94|95)", communes_2022_2$COM)
] # Toutes les communes d'IdF sauf celles de Seine-et-Marne

# On fusionne tout
communes_20h <- c(
  paca_20h, na_20h, norm_20h, aura_20h, pdll_20h, ge_20h, occ_20h, idf_20h
)

# Ajout des horaires à la base
communes_2022_3 <- communes_2022_2 %>%
  mutate(
    FERMETURE = ifelse(COM %in% communes_20h, "20h", "19h")
  )

communes_2022_3$FERMETURE <- factor(communes_2022_3$FERMETURE,
                                    levels = c("19h", "20h"))


################################################################################
################################ Fusion ########################################
################################################################################

setdiff(bv_2022_final_3$COM, communes_2022_3$COM)
setdiff(communes_2022_3$COM, bv_2022_final_3$COM)

# Six communes, toutes situées dans la Meuse, n'ont pas de bureaux de vote.
# On fusionne avec l'heure de fermeture des bureaux de vote en faisant donc un
# left_join sur bv_2022_final_3.

glimpse(bv_2022_final_3)
glimpse(communes_2022_3)

# En plus de l'heure de fermeture, on ajoute le nom du département et de la
# région
bv_2022_final_4 <- bv_2022_final_3 %>%
  left_join(
    communes_2022_3 %>%
      dplyr::select(COM, REG, REG_LIB, DEP_LIB, FERMETURE),
    by = "COM"
  ) %>% 
  dplyr::select(ID, REG, REG_LIB, DEP, DEP_LIB, COM, COM_LIB, BV, FERMETURE)

glimpse(bv_2022_final_4)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_4,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_4.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

rm(list = ls())

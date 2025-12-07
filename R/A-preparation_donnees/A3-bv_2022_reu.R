################################################################################
############################ Importation des données ###########################
################################################################################


objets_initiaux <- ls()

bv_sept2022_reu_1 <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "/sources/bv_sept2022_reu.csv",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = ""),
    show_col_types = FALSE
  )

glimpse(bv_sept2022_reu_1)

bv_mars2022_reu_1 <- aws.s3::s3read_using(
  FUN = readr::read_delim,
  delim = "\t",
  object = "/sources/bv_mars2022_reu.csv",
  bucket = "projet-ensai-methodo-3a",
  opts = list("region" = ""),
  show_col_types = FALSE,
  col_types = cols(code_postal = col_character())
)

glimpse(bv_mars2022_reu_1)

bv_2022_final_2 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_2.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_2)



################################################################################
############################ Nettoyage des bases ###############################
################################################################################


unique(substr(bv_sept2022_reu_1$code_commune, 1, 2))

bv_sept2022_reu_2 <- bv_sept2022_reu_1 %>% 
  filter(!(substr(code_commune, 1, 2) == "97")) %>%  # On retire les Outre-mer
  dplyr::select(id_brut_miom, id_brut_reu, id_brut_insee, code_commune,
                nb_adresses_initial) %>% 
  rename(ID_REU = id_brut_reu,
         ID_MIOM = id_brut_miom,
         ID_INSEE = id_brut_insee,
         COM = code_commune,
         NB_ADRS = nb_adresses_initial) %>% 
  mutate(BV_BRUT = sub(".*_", "", ID_INSEE)) %>% 
  arrange(ID_REU)

bv_mars2022_reu_2 <- bv_mars2022_reu_1 %>%
  mutate(ID_INSEE = paste0(commune_code, "_", code_normalise_complet),
         commune_code = ifelse(nchar(commune_code) == 4, 
                               paste0("0", commune_code), 
                               commune_code))

unique(substr(bv_mars2022_reu_2$commune_code, 1, 2))

bv_mars2022_reu_3 <- bv_mars2022_reu_2 %>% 
  filter(!(substr(commune_code, 1, 2) %in% c("97", "98"))) %>% 
  dplyr::select(commune_code, code_normalise_complet, ID_INSEE) %>% 
  rename(COM = commune_code,
         BV_BRUT = code_normalise_complet) %>% 
  mutate(ID_REU = paste0(COM, "_", BV_BRUT),
         ID_MIOM = paste0(COM, "_", str_pad(BV_BRUT, width = 4, pad = "0")))

# Faire appariement entre bv_sept et bv_mars avec le ID_INSEE, puis entre
# bv_final et bv_sept ou bv_mars avec l'ID MIOM

setdiff(bv_sept2022_reu_2$ID_MIOM, bv_2022_final_2$ID)

sum(is.na(bv_sept2022_reu_2$ID_MIOM))

bv_sept2022_reu_2 %>%
  filter(is.na(ID_MIOM)) %>%
  print(n = 124)

# 124 (NA) + 55 bureaux de vote sont dans bv_sept2022_reu_2 sans être dans
# bv_2022_final_1, ce qui est anormal. En réalité, on remarque que cela semble
# entièrement dû à un problème d'encodage de l'id de ces bureaux de vote dans la
# base bv_sept2022_reu_2
# - pour les 55 qui n'ont pas de NA dans i_brut_miom, cela semble dû à l'absence
#   des 4 chiffres. Or, le code du bureau de vote est bien donné. On ajouterait
#   ainsi les "0" manquants à la gauche du numéro du bureau de vote déjà
#   présent, et ce pour arriver à 4 chiffes en tout après le tiret du 8.
# - pour les 124 qui ont un NA pour leur id_brut_miom, 123 ont pourtant bien un
#   code pour leur bureau de vote dans la variable correspondante. Comme pour
#   les 55 bureaux présentant l'autre problème, il est également nécessaire
#   d'ajouter des "0" et pas seulement de faire une simple concaténation.
#   En revanche, un bureau de vote à un NA dans la variable code : celui de
#   la commune de Fornex. Or, il s'agit d'un bureau de vote unique. Nous lui
#   attribuons ainsi le numéro 1 (c'est bien le code donné dans la base des
#   des résultats électoraux).

# Faisons ces deux corrections et voyons ensuite s'il reste des bureaux de
# bv_2022_reu_2 qui ne sont pas présent dans bv_2022_final_1.

bureaux_anormaux <- setdiff(bv_sept2022_reu_2$ID_MIOM, bv_2022_final_2$ID)
bureaux_anormaux <- bureaux_anormaux[!is.na(bureaux_anormaux)]

# Corriger le format en ajoutant des zéros à gauche après le "_"
bv_sept2022_reu_3 <- bv_sept2022_reu_2
bv_sept2022_reu_3$ID_MIOM[bv_sept2022_reu_3$ID_MIOM %in% bureaux_anormaux] <- 
  sapply(strsplit(bureaux_anormaux, "_"), function(x) {
    paste0(x[1], "_", sprintf("%04d", as.numeric(x[2])))
  })

# Vérifications
setdiff(bv_sept2022_reu_3$ID_MIOM, bv_2022_final_2$ID)
# Il ne reste plus que les NA

bv_sept2022_reu_3 %>%
  filter(is.na(ID_MIOM)) %>%
  print(n = 124)

bv_sept2022_reu_4 <- bv_sept2022_reu_3 %>%
  mutate(
    ID_MIOM = case_when(
      grepl("^10387_", ID_REU) ~ paste0("10387_", sprintf("%04d", cumsum(grepl("^10387_", ID_REU)))), # Troyes
      ID_REU == "15014_C09"      ~ "15014_0009",
      ID_REU == "2B166_1 - 1"    ~ "2B166_0001",
      ID_REU == "2B166_2 - 3"    ~ "2B166_0002",
      grepl("^76217_", ID_REU) ~ paste0("76217_", sprintf("%04d", cumsum(grepl("^76217_", ID_REU)))), # Dieppe
      grepl("^90010_", ID_REU) ~ paste0("90010_", sprintf("%04d", cumsum(grepl("^90010_", ID_REU)))), # Belfort
      .default = ID_MIOM
    )
  )

bv_2022_final_3 <- bv_2022_final_2 %>%
  mutate(
    ID = case_when(
      ID == "16286_0005"  ~ "16286_0003",
      .default = ID
    )
  )

setdiff(bv_sept2022_reu_4$ID_MIOM, bv_2022_final_3$ID)

bv_sept2022_reu_4 %>%
  filter(is.na(ID_MIOM)) %>%
  print(n = 124)


test <- bv_2022_final_2 %>%
  filter(INSCRITS_T1 < 5 | INSCRITS_T2 < 5)


################################################################################
################################ Fusion ########################################
################################################################################

bv_2022_final_3 <- bv_2022_final_2

glimpse(bv_2022_final_3)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_3,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_3.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

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

bv_2022_final_2.1 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_2.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_2.1)

cog2022_1 <-
  aws.s3::s3read_using(
    FUN = read.csv,
    object = "/sources/communes_2022.csv",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(cog2022_1)

adresses_2022_reu <- arrow::open_dataset(
  sources = "s3://projet-ensai-methodo-3a/sources/adresses_2022_reu.parquet",
  format = "parquet",
  filesystem = "s3"
)

glimpse(adresses_2022_reu)



################################################################################
########################### Nettoyage de bv_sept2022_reu #######################
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



################################################################################
##### Etude de la correspondance entre bv_sept2022_reu etbv_2022_final_2 #######
################################################################################


setdiff(bv_sept2022_reu_2$ID_MIOM, bv_2022_final_2.1$ID)
setdiff(bv_2022_final_2.1$ID, bv_sept2022_reu_2$ID_MIOM)

# ----------------------------
# Problème des bureaux présents dans bv_sept2022_reu mais absents de bv_2022_final
# Il s'agit des "nouveaux bureaux", créés entre la présidentielle et l'extraction du REU
# ----------------------------

setdiff(bv_sept2022_reu_2$ID_MIOM, bv_2022_final_2.1$ID)

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
#   ainsi les "0" manquants à la gauche du numéro du bureau de vote déjà. On
#   appelle ces bureaux les "bureaux_anormaux".
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

bureaux_anormaux <- setdiff(bv_sept2022_reu_2$ID_MIOM, bv_2022_final_2.1$ID)
bureaux_anormaux <- bureaux_anormaux[!is.na(bureaux_anormaux)]

# Corriger le format en ajoutant des zéros à gauche après le "_"
bv_sept2022_reu_3 <- bv_sept2022_reu_2
bv_sept2022_reu_3$ID_MIOM[bv_sept2022_reu_3$ID_MIOM %in% bureaux_anormaux] <- 
  sapply(strsplit(bureaux_anormaux, "_"), function(x) {
    paste0(x[1], "_", sprintf("%04d", as.numeric(x[2])))
  })

# Vérifications
setdiff(bv_sept2022_reu_3$ID_MIOM, bv_2022_final_2.1$ID)
# Il ne reste plus que les NA

bv_sept2022_reu_3 %>%
  filter(is.na(ID_MIOM)) %>%
  print(n = 124)

# Il est possilbe de corriger certains bureaux visuellement
bv_sept2022_reu_4 <- bv_sept2022_reu_3 %>%
  mutate(
    ID_MIOM = case_when(
      grepl("^10387_", ID_REU) ~ paste0("10387_", sprintf("%04d", cumsum(grepl("^10387_", ID_REU)))), # Troyes
      ID_REU == "15014_C09"      ~ "15014_0009",
      ID_REU == "2B166_1 - 1"    ~ "2B166_0001",
      ID_REU == "2B166_2 - 3"    ~ "2B166_0002",
      grepl("^76217_", ID_REU) ~ {
        current_num <- cumsum(grepl("^76217_", ID_REU))
        adjusted_num <- ifelse(current_num >= 7, current_num + 1, current_num)
        paste0("76217_", sprintf("%04d", adjusted_num))
      }, # Dieppe (saute 0007)
      grepl("^90010_", ID_REU) ~ paste0("90010_", sprintf("%04d", cumsum(grepl("^90010_", ID_REU)))), # Belfort
      ID_REU == "16286_3"    ~ "16286_0003",
      .default = ID_MIOM
    )
  )

bv_2022_final_2.2 <- bv_2022_final_2.1 %>%
  mutate(
    ID = case_when(
      ID == "16286_0005"  ~ "16286_0003",
      .default = ID
    )
  )

setdiff(bv_sept2022_reu_4$ID_MIOM, bv_2022_final_2.2$ID)
# Les NA qui restent sont les bureaux qui ont été créés entre la présidentielle
# et l'extraction du REU
nx_bv <- bv_sept2022_reu_4 %>%
  filter(is.na(ID_MIOM)) %>% 
  pull(ID_REU)

# On les supprime de bv_sept2022_reu
bv_sept2022_reu_5 <- bv_sept2022_reu_4 %>% 
  filter(!(ID_REU %in% nx_bv))

setdiff(bv_sept2022_reu_5$ID_MIOM, bv_2022_final_2.2$ID) # tout va bien

# ----------------------------
# Problème des bureaux présents dans bv_2022_final mais absents de bv_sept2022_reu
# Il s'agit soit des bureaux des prisonniers (ou autres cas particuliers), soit
# des bureaux supprimés entre la présidentielle et l'extraction du REU
# ----------------------------

setdiff(bv_2022_final_2.2$ID, bv_sept2022_reu_5$ID_MIOM)
bv_soupcon_suppression <- setdiff(bv_2022_final_2.2$ID, bv_sept2022_reu_5$ID_MIOM)

bv_prisonniers <- c("75056_JUS1")

base_bv_soupcon_suppression <- bv_2022_final_2.2 %>% 
  filter(ID %in% bv_soupcon_suppression) %>% 
  mutate(TAUX_PARTICIPATION_T1 = round(VOTANTS_T1 / INSCRITS_T1 * 100, 1), .after = EXPRIMES_T1)

base_bv_prisonniers <- base_bv_soupcon_suppression %>% 
  filter(TAUX_PARTICIPATION_T1 <= 60 | INSCRITS_T1 < 5)
  
bv_prisonniers <- c(bv_prisonniers, base_bv_prisonniers %>% 
  pull(ID))

bv_2022_final_2.3 <- bv_2022_final_2.2 %>% 
  filter(!(ID %in% bv_prisonniers))

setdiff(bv_2022_final_2.3$ID, bv_sept2022_reu_5$ID_MIOM)

bv_sept2022_reu_6 <- bv_sept2022_reu_5 %>% 
  mutate(ID_MIOM = case_when(
    ID_REU == "59508_3" ~ "59508_0103", # problème encodage
    .default = ID_MIOM
  ))

setdiff(bv_2022_final_2.3$ID, bv_sept2022_reu_6$ID_MIOM)
# 8 bv ont été supprimés, à Monclart il n'y a pas d'adresses...

bv_supprimes <- setdiff(bv_2022_final_2.3$ID, bv_sept2022_reu_6$ID_MIOM)



################################################################################
############################## Problème des doublons ###########################
################################################################################


length(unique(bv_2022_final_2.3$ID)) == nrow(bv_2022_final_2.3)
length(unique(bv_sept2022_reu_6$ID_MIOM)) == nrow(bv_sept2022_reu_6)
bv_sept2022_reu_6$ID_MIOM[duplicated(bv_sept2022_reu_6$ID_MIOM)]

bv_sept2022_reu_7 <- bv_sept2022_reu_6 %>%
  arrange(ID_MIOM, desc(NB_ADRS)) %>%
  group_by(ID_MIOM) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(
    bv_sept2022_reu_6 %>%
      group_by(ID_MIOM) %>%
      summarise(NB_ADRS_sup = sum(NB_ADRS) - first(NB_ADRS)) %>%
      ungroup(),
    by = "ID_MIOM"
  ) %>%
  mutate(NB_ADRS = NB_ADRS + NB_ADRS_sup) %>%
  select(-NB_ADRS_sup)

length(unique(bv_sept2022_reu_7$ID_MIOM)) == nrow(bv_sept2022_reu_7)

bv_2022_final_3 <- bv_2022_final_2.3

glimpse(bv_2022_final_3)


################################################################################
########################### Vérifications COG ##################################
################################################################################


cog2022_2 <- cog2022_1 %>% 
  filter(TYPECOM %in% c("COM"),
         !(substr(COM, 1, 2) == "97"))

setdiff(bv_2022_final_3$COM, cog2022_2$COM)
setdiff(cog2022_2$COM, bv_2022_final_3$COM) # communes_zero_habitants
communes_zero_habitants <- setdiff(cog2022_2$COM, bv_2022_final_3$COM)

setdiff(bv_sept2022_reu_6$COM, cog2022_2$COM) # arrondissements de PLM
setdiff(setdiff(cog2022_2$COM, bv_sept2022_reu_6$COM), communes_zero_habitants)
# on retrouve les codes Insee de PLM + la commune de Monclart (04126) qui n'est
# pas dans le REU de septembre 2025

# Tout est ok



################################################################################
############################ Corrections REU ###################################
################################################################################






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


bv_2022_final_3 %>% 
  filter(ID == (bv_sept2022_reu_7 %>% filter(ID_REU == "01001_1") %>% pull(ID_MIOM))) %>% 
  pull(INSCRITS_T1)

  
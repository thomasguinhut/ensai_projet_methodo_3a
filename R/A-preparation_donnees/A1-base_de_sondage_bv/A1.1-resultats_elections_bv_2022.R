################################################################################
############################ Importation des données ###########################
################################################################################

resultats_2022_t1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "projet_methodo_3a/resultats_2022_t1_bv.xlsx",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(resultats_2022_t1)

resultats_2022_t2_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "projet_methodo_3a/resultats_2022_t2_bv.xlsx",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(resultats_2022_t2_1)

resultats_2022_t2_2 <- resultats_2022_t2_1 %>% 
  dplyr::select(`Code du département`, `Code de la commune`, `Code du b.vote`,
                Inscrits, Votants, Exprimés, Voix, ...33,
                `Libellé de la commune`) %>% 
  mutate(COM = paste0(`Code du département`, `Code de la commune`),
         ID = paste0(COM, "_", `Code du b.vote`)) %>% 
  rename(DEP = `Code du département`,
         BV = `Code du b.vote`,
         INSCRITS = Inscrits,
         VOTANTS = Votants,
         EXPRIMES = Exprimés,
         MACRON = Voix,
         LEPEN = ...33,
         NOM_COM = `Libellé de la commune`) %>% 
  dplyr::select(ID, DEP, COM, NOM_COM, BV, INSCRITS, VOTANTS, EXPRIMES, MACRON,
                LEPEN)

unique(resultats_2022_t2_2$COM)

unique(bdd$COM)

setdiff(unique(resultats_2022_t2_2$COM), unique(bdd$COM))

# Il faut filter à la métropole

# test <- resultats_2022_t2_2 %>% 
#   left_join(bdd,  by = "COM")

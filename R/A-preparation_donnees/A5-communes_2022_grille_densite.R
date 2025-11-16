################################################################################
############################ Importation des données ###########################
################################################################################

objets_initiaux <- ls()

grille_densite_1 <-
  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = "/sources/grille_densite_2025_cog2022.xlsx",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(grille_densite_1)

bv_2022_final_4 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "/export_bv_finaux/bv_2022_final_4.rds",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )

glimpse(bv_2022_final_4)


################################################################################
############################ Nettoyage des bases ###############################
################################################################################

names(grille_densite_1) <- as.character(grille_densite_1[4,])
grille_densite_2 <- grille_densite_1[-c(1:4),]

grille_densite_3 <- grille_densite_2 %>% 
  filter(!(substr(CODGEO, 1, 2) == "97")) %>% # On retire les Outre-mer
  dplyr::select(CODGEO, DENS, LIBDENS, P1, P2, P3, DENS_AAV, LIBDENS_AAV,
                DENS7, LIBDENS7) %>% 
  rename(COM = CODGEO,
         DENS3 = DENS,
         DENS3_P1 = P1,
         DENS3_P2 = P2,
         DENS3_P3 = P3,
         DENS3_LIB = LIBDENS,
         DENS4 = DENS_AAV,
         DENS4_LIB = LIBDENS_AAV,
         DENS7_LIB = LIBDENS7)

grille_densite_3$DENS3_P1 <- as.numeric(grille_densite_3$DENS3_P1)
grille_densite_3$DENS3_P2 <- as.numeric(grille_densite_3$DENS3_P2)
grille_densite_3$DENS3_P3 <- as.numeric(grille_densite_3$DENS3_P3)

grille_densite_3$DENS3_LIB <- factor(
  grille_densite_3$DENS3_LIB, levels = c("Urbain dense", "Urbain intermédiaire",
                                         "Rural")
)
grille_densite_3$DENS4_LIB <- factor(
  grille_densite_3$DENS4_LIB, levels = c("Urbain dense", "Urbain intermédiaire",
                                         "Rural périurbain",
                                         "Rural non périurbain")
)
grille_densite_3$DENS7_LIB <- factor(
  grille_densite_3$DENS7_LIB, levels = c("Grands centres urbains",
                                         "Centres urbains intermédiaires",
                                         "Ceintures urbaines", "Petites villes",
                                         "Bourgs ruraux",
                                         "Rural à habitat dispersé",
                                         "Rural à habitat très dispersé")
)

glimpse(grille_densite_3)

setdiff(bv_2022_final_4$COM, grille_densite_3$COM)
# Aucune commune de la base des bureaux de vote n'est absente de la grille de
# densité.


################################################################################
################################ Fusion ########################################
################################################################################

bv_2022_final_5 <- bv_2022_final_4 %>% 
  left_join(grille_densite_3, by = "COM")

glimpse(bv_2022_final_5)


################################################################################
################################ Export ########################################
################################################################################

aws.s3::s3write_using(
  bv_2022_final_5,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "/export_bv_finaux/bv_2022_final_5.rds",
  bucket = "projet-ensai-methodo-3a",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)

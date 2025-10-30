glimpse(bv_2022_final)

names(bv_2022_final)

round(prop.table(table(bv_2022$FERMETURE)) * 100, 1)

round(
  prop.table(table(bv_2022$REG_LIB, bv_2022$FERMETURE), margin = 1) * 100,
  1
)

round(
  prop.table(table(bv_2022$DEP_LIB, bv_2022$FERMETURE), margin = 1) * 100,
  1
)

round(
  prop.table(table(bv_2022$DENS7_LIB, bv_2022$FERMETURE), margin = 1) * 100,
  1
)

hist(bv_2022$INSCRITS_T1)

glimpse(data.frame(bv_2022_final))

round(prop.table(table(bv_2022_final$FERMETURE)) * 100, 1)

round(
  prop.table(table(bv_2022_final$REG_LIB, bv_2022_final$FERMETURE), margin = 1) * 100,
  1
)

round(prop.table(table(bv_2022_final$DEP_LIB, bv_2022_final$FERMETURE), margin = 1) * 100, 1)[order(-round(prop.table(table(bv_2022_final$DEP_LIB, bv_2022_final$FERMETURE), margin = 1) * 100, 1)[, "20h"]), ]


round(
  prop.table(table(bv_2022_final$DENS7_LIB, bv_2022_final$FERMETURE), margin = 1) * 100,
  1
)

hist(bv_2022_final$INSCRITS_T1)


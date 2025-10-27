round(prop.table(table(bdd_2$FERMETURE_20h)) * 100, 1)

round(
  prop.table(table(bdd_2$NOM_REG, bdd_2$FERMETURE_20h), margin = 1) * 100,
  1
)

round(
  prop.table(table(bdd_2$NOM_DEP, bdd_2$FERMETURE_20h), margin = 1) * 100,
  1
)
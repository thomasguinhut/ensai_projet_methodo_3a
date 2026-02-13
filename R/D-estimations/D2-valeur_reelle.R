valeur_reelle <- function(ech, candidat, tour = "T1") {
  
  valeur_reelle_pct <- round(
    (sum(bv_2022_final[[paste0(candidat, "_", tour)]]) / sum(
      bv_2022_final[[paste0("EXPRIMES_", tour)]]) * 100
    ), 2)
  
  return(valeur_reelle_pct)
  
}
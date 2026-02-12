estimation_brute <- function(ech, candidat, methode) {
  
  estimation_pct <- round(
    (sum(ech[[candidat]] * ech[[paste0("poids_", methode)]]) /
       sum(ech[[paste0("poids_", methode)]])) * 100, 2
  )
  
  return(estimation_pct)
  
}
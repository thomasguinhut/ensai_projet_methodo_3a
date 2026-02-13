estimation_brute <- function(ech, candidat) {
  
  estimation_pct <- round(
    (sum(ech[[candidat]] * ech$poids) /
       sum(ech$poids)) * 100, 2
  )
  
  return(estimation_pct)
  
}
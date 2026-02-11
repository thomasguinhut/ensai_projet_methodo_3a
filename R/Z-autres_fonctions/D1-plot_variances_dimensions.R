plot_variances_dimensions <- function(res){
  
  res$eig %>%
    as.data.frame() %>%
    mutate(
      PC = row_number(),
      var = `percentage of variance`,
      cum = `cumulative percentage of variance`,
      cum_scaled = cum / max(cum) * max(var)
    ) %>%
    ggplot(aes(x = PC)) +
    geom_col(aes(y = var),
             fill = "steelblue") +
    geom_text(
      aes(y = var, label = round(var, 0)),
      vjust = -0.5,
      color = "steelblue",
      size = 3
    ) +
    geom_line(aes(y = cum_scaled),
              color = "red") +
    geom_point(aes(y = cum_scaled),
               color = "red") +
    geom_text(
      aes(y = cum_scaled, label = round(cum, 0)),
      vjust = -0.8,
      color = "red",
      size = 3
    ) +
    scale_y_continuous(
      name = "Percentage of variances\n",
      sec.axis = sec_axis(
        ~ . * max(res$eig[,3]) / max(res$eig[,2]),
        name = "Cumulative percentage of variances\n"
      )
    ) +
    labs(
      title = "Variances explained\n",
      x = "\nPrincipal Components"
    ) +
    theme_minimal()
  
}
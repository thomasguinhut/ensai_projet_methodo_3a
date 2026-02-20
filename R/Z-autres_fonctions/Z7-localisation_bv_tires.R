localisation_bv_tires <- function(ech) {
  
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
  
  codes_avec_compte <- ech %>%
    mutate(code_com = substr(ID, 1, 5)) %>%
    group_by(code_com) %>%
    summarise(nb_bv = n_distinct(ID), .groups = "drop")
  
  codes_uniques <- codes_avec_compte$code_com
  
  get_info <- function(code_com) {
    tryCatch({
      api_url  <- paste0("https://geo.api.gouv.fr/communes/", code_com,
                         "?fields=nom,population&format=json")
      res  <- httr::GET(api_url)
      dat  <- httr::content(res)
      if (is.null(dat$nom)) return(NULL)
      data.frame(
        code_com   = code_com,
        commune    = as.character(dat$nom),
        population = as.integer(dat$population %||% NA)
      )
    }, error = function(e) NULL)
  }
  
  get_coords <- function(code_com) {
    tryCatch({
      api_url <- paste0("https://geo.api.gouv.fr/communes/", code_com,
                        "?fields=centre&format=json")
      dat <- httr::content(httr::GET(api_url))
      if (is.null(dat$centre)) return(NULL)
      data.frame(code_com = code_com,
                 lon = dat$centre$coordinates[[1]],
                 lat = dat$centre$coordinates[[2]])
    }, error = function(e) NULL)
  }
  
  infos  <- bind_rows(lapply(codes_uniques, get_info))
  coords <- bind_rows(lapply(codes_uniques, get_coords))
  
  tableau <- infos %>%
    left_join(codes_avec_compte, by = "code_com") %>%
    arrange(desc(population)) %>% 
    arrange(desc(nb_bv))
  
  print(tableau)
  
  france_map <- ggplot2::map_data("france")
  
  p <- ggplot() +
    geom_polygon(data = france_map, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "grey60") +
    geom_point(data = coords, aes(x = lon, y = lat),
               color = "red", size = 3) +
    ggrepel::geom_text_repel(
      data = coords %>% left_join(infos, by = "code_com"),
      aes(x = lon, y = lat, label = commune),
      size = 1.5,
      segment.color = "grey40",
      segment.size  = 0.3,
      max.overlaps  = Inf
    ) +
    coord_fixed(1.65) +
    theme_void() +
    labs(title = paste0("Localisation des communes (", nrow(coords), " communes, ",
                        nrow(ech), " bureaux de vote)"))
  
  print(p)

}
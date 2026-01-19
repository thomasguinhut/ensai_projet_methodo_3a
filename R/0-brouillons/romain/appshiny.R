# library(shiny)
# library(sf)
# library(dplyr)
# library(leaflet)
# library(aws.s3)
# library(arrow)
# library(stringr)
# 
# departements <- c("01","02","03","04","05","06","07","08","09","10",
#                   "11","12","13","14","15","16","17","18","19","21",
#                   "22","23","24","25","26","27","28","29","2A","2B",
#                   "30","31","32","33","34","35","36","37","38","39",
#                   "40","41","42","43","44","45","46","47","48","49",
#                   "50","51","52","53","54","55","56","57","58","59",
#                   "60","61","62","63","64","65","66","67","68","69",
#                   "70","71","72","73","74","75","76","77","78","79",
#                   "80","81","82","83","84","85","86","87","88","89",
#                   "90","91","92","93","94","95")
# 
# #---------------------------------------
# #   CACHES POUR EVITER LES RE-LECTURES
# #---------------------------------------
# cache_bdv <- list()
# cache_carreaux <- list()
# 
# ui <- fluidPage(
#   titlePanel("Visualisation optimisée : BDV + Filosofi + adresses"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("dept_select", "Choisir un département :", choices = departements),
#       checkboxInput("show_carreaux", "Afficher les carreaux Filosofi", FALSE),
#       uiOutput("var_select_ui")
#     ),
#     
#     mainPanel(
#       leafletOutput("map_leaflet", height = "800px")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   #-----------------------------
#   # 1) Chargement unique adresses
#   #-----------------------------
#   
#   adresses_coords <- aws.s3::s3read_using(
#     FUN = arrow::read_parquet,
#     object = "sources/table-adresses-reu.parquet",
#     bucket = "projet-ensai-methodo-3a",
#     opts = list("region" = "")
#   ) %>% 
#     mutate(dept = str_sub(code_commune_ref, 1, 2))
#   
#   adresses_sf <- st_as_sf(adresses_coords,
#                           coords = c("longitude", "latitude"),
#                           crs = 4326,
#                           remove = FALSE)
#   
#   #-----------------------------
#   # 2) UI dynamique variables Filosofi
#   #-----------------------------
#   
#   output$var_select_ui <- renderUI({
#     req(input$dept_select, input$show_carreaux)
#     
#     carreaux_file <- paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_",
#                             input$dept_select, ".gpkg")
#     
#     if (!file.exists(carreaux_file)) return()
#     
#     # Lecture en cache
#     if (is.null(cache_carreaux[[input$dept_select]])) {
#       
#       c_temp <- st_read(carreaux_file, quiet = TRUE)
#       
#       # ➜ Correction : transformation en WGS84 AVANT mise en cache
#       c_temp <- st_transform(c_temp, 4326)
#       
#       cache_carreaux[[input$dept_select]] <<- c_temp
#     }
#     
#     vars <- names(cache_carreaux[[input$dept_select]])
#     
#     selectInput("var_select", "Variable pour le dégradé :", choices = vars)
#   })
#   
#   #-----------------------------
#   # 3) Carte initiale
#   #-----------------------------
#   
#   output$map_leaflet <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles("CartoDB.Positron") %>%
#       setView(lng = 2, lat = 46.5, zoom = 6)
#   })
#   
#   #-----------------------------
#   # 4) BDV optimisés (cache)
#   #-----------------------------
#   
#   observeEvent(input$dept_select, {
#     
#     bdv_file <- paste0("R/0-brouillons/romain/bdv_departements/bdv_dep_",
#                        input$dept_select, ".gpkg")
#     
#     if (!file.exists(bdv_file)) return()
#     
#     if (is.null(cache_bdv[[input$dept_select]])) {
#       bdv <- st_read(bdv_file, quiet = TRUE)
#       bdv <- st_transform(bdv, 4326)
#       cache_bdv[[input$dept_select]] <<- bdv
#     }
#     
#     bdv <- cache_bdv[[input$dept_select]]
#     
#     leafletProxy("map_leaflet") %>%
#       clearGroup("BDV") %>%
#       addPolylines(
#         data = bdv,
#         color = "blue", weight = 2,
#         fillColor = "lightblue",
#         fillOpacity = 0.4,
#         group = "BDV"
#       ) %>%
#       fitBounds(st_bbox(bdv)[1], st_bbox(bdv)[2],
#                 st_bbox(bdv)[3], st_bbox(bdv)[4])
#   })
#   
#   #-----------------------------
#   # 5) Carreaux optimisés (cache + simplify)
#   #-----------------------------
#   observeEvent(c(input$show_carreaux, input$var_select), {
#     
#     leafletProxy("map_leaflet") %>% clearGroup("Carreaux") %>% clearControls()
#     if (!input$show_carreaux) return()
#     
#     carreaux <- cache_carreaux[[input$dept_select]]
#     if (is.null(carreaux)) return()
#     
#     var <- input$var_select
#     req(var)
#     
#     # Extraire la variable
#     v <- suppressWarnings(as.numeric(carreaux[[var]]))
#     
#     # Vérification stricte
#     if (all(is.na(v))) {
#       showNotification("Impossible d'afficher : variable non numérique", type = "error")
#       return()
#     }
#     
#     # Palette
#     pal <- colorNumeric(
#       palette = "viridis",
#       domain = v,
#       na.color = "transparent",
#       reverse = FALSE
#     )
#     
#     # Ajout des carreaux colorés
#     leafletProxy("map_leaflet") %>%
#       addPolygons(
#         data = carreaux,
#         color = "black", weight = 0.3,
#         fillColor = ~pal(v),
#         fillOpacity = 0.7,
#         group = "Carreaux",
#         label = ~paste0(var, " : ", round(v, 1)),
#         highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.9)
#       ) %>%
#       
#       # Légende dynamique
#       addLegend(
#         position = "bottomright",
#         pal = pal,
#         values = v,
#         title = paste("Variable :", var),
#         opacity = 1
#       )
#   })
#   
#   
#   #-----------------------------
#   # 6) Adresses (cluster)
#   #-----------------------------
#   
#   observeEvent(input$dept_select, {
#     
#     leafletProxy("map_leaflet") %>% clearGroup("Adresses")
#     
#     adresses_dept <- adresses_sf %>% filter(dept == input$dept_select)
#     
#     leafletProxy("map_leaflet") %>%
#       addCircleMarkers(
#         data = adresses_dept,
#         lng = ~longitude, lat = ~latitude,
#         color = "green",
#         radius = 4,
#         fillOpacity = 0.8,
#         clusterOptions = markerClusterOptions(),  # ⭐ accélération énorme
#         group = "Adresses"
#       )
#   })
#   
# }
# 
# shinyApp(ui = ui, server = server)

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(leafgl)
library(aws.s3)
library(arrow)
library(stringr)
library(viridis)

# départements
departements <- c("01","02","03","04","05","06","07","08","09","10",
                  "11","12","13","14","15","16","17","18","19","21",
                  "22","23","24","25","26","27","28","29","2A","2B",
                  "30","31","32","33","34","35","36","37","38","39",
                  "40","41","42","43","44","45","46","47","48","49",
                  "50","51","52","53","54","55","56","57","58","59",
                  "60","61","62","63","64","65","66","67","68","69",
                  "70","71","72","73","74","75","76","77","78","79",
                  "80","81","82","83","84","85","86","87","88","89",
                  "90","91","92","93","94","95")

# caches
cache_bdv <- list()
cache_carreaux <- list()

ui <- fluidPage(
  titlePanel("Carreaux WebGL + clic → panneau d'info"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dept_select", "Choisir un département :", choices = departements),
      checkboxInput("show_carreaux", "Afficher les carreaux Filosofi (WebGL)", FALSE),
      uiOutput("var_select_ui"),
      hr(),
      h4("Info carreau (clic)"),
      htmlOutput("carreau_info", container = div)  # panneau d'info
    ),
    
    mainPanel(
      leafletOutput("map_leaflet", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # 1) chargement unique des adresses 
  adresses_coords <- aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "sources/table-adresses-reu.parquet",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  ) %>% mutate(dept = str_sub(code_commune_ref, 1, 2))
  adresses_sf <- st_as_sf(adresses_coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # 2) UI dynamique variables Filosofi 
  output$var_select_ui <- renderUI({
    req(input$dept_select, input$show_carreaux)
    carreaux_file <- paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_", input$dept_select, ".gpkg")
    if (!file.exists(carreaux_file)) return()
    # lire + transformer + simplifier une seule fois et mettre en cache
    if (is.null(cache_carreaux[[input$dept_select]])) {
      tmp <- st_read(carreaux_file, quiet = TRUE)
      tmp <- st_transform(tmp, 4326)
      # simplification raisonnable (garde la topologie)
      tmp <- st_simplify(tmp, dTolerance = 1)
      # ensure an ID column for quick lookup
      if (!(".__id__" %in% names(tmp))) tmp$.__id__ <- seq_len(nrow(tmp))
      cache_carreaux[[input$dept_select]] <<- tmp
    }
    vars <- names(cache_carreaux[[input$dept_select]])
    selectInput("var_select", "Variable pour le dégradé :", choices = vars, selected = vars[1])
  })
  
  #  3) carte initiale 
  output$map_leaflet <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2, lat = 46.5, zoom = 6)
  })
  
  #  4) BDV cache et affichage simple (polylines) 
  observeEvent(input$dept_select, {
    bdv_file <- paste0("R/0-brouillons/romain/bdv_departements/bdv_dep_", input$dept_select, ".gpkg")
    if (!file.exists(bdv_file)) return()
    if (is.null(cache_bdv[[input$dept_select]])) {
      bdv <- st_read(bdv_file, quiet = TRUE)
      bdv <- st_transform(bdv, 4326)
      cache_bdv[[input$dept_select]] <<- bdv
    }
    bdv <- cache_bdv[[input$dept_select]]
    leafletProxy("map_leaflet") %>%
      clearGroup("BDV") %>%
      addPolylines(data = bdv, color = "blue", weight = 2, group = "BDV") %>%
      fitBounds(st_bbox(bdv)[1], st_bbox(bdv)[2], st_bbox(bdv)[3], st_bbox(bdv)[4])
  })
  
  # 5) Carreaux WebGL (leafgl) 
  observeEvent(c(input$show_carreaux, input$var_select, input$dept_select), {
    # clear any previous visualization
    leafletProxy("map_leaflet") %>% clearGroup("Carreaux_GL") %>% clearControls()
    if (!isTRUE(input$show_carreaux)) return()
    carreaux <- cache_carreaux[[input$dept_select]]
    if (is.null(carreaux)) return()
    var <- input$var_select
    req(var)
    
    v <- suppressWarnings(as.numeric(carreaux[[var]]))
    if (all(is.na(v))) {
      showNotification("Variable non numérique ou vide pour ce département", type = "error")
      return()
    }
    
    pal_vals <- v
    pal_vals[is.na(pal_vals)] <- min(pal_vals, na.rm = TRUE) # avoid NA in scale
    cols <- viridis(256)[as.integer(cut(pal_vals, breaks = 256, labels = FALSE))]
    
    carreaux$.__color__ <- cols
    
    leafletProxy("map_leaflet") %>%
      leafgl::addGlPolygons(data = carreaux,
                            color = ~.__color__, # stroke color
                            fillColor = ~.__color__,
                            weight = 0.2,
                            opacity = 1,
                            fillOpacity = 0.8,
                            group = "Carreaux_GL")
    
    pal <- colorNumeric("viridis", domain = v, na.color = "transparent")
    leafletProxy("map_leaflet") %>%
      addLegend(position = "bottomright", pal = pal, values = v, title = paste("Variable:", var))
  })
  
  # 6) Click handling: find carreau containing click and display info 
  observeEvent(input$map_leaflet_click, {
    click <- input$map_leaflet_click
    if (is.null(click)) return()
    lon <- click$lng; lat <- click$lat
    pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
    # ensure carreaux for current dept loaded
    carreaux <- cache_carreaux[[input$dept_select]]
    if (is.null(carreaux)) {
      output$carreau_info <- renderUI(HTML("<i>Carreaux non chargés</i>"))
      return()
    }
    # use a fast spatial index: st_intersects
    # convert point to sf and perform intersection
    pt_sf <- st_sf(geometry = pt)
    hit_idx <- which(st_intersects(pt_sf, carreaux, sparse = FALSE)[1, ])
    if (length(hit_idx) == 0) {
      output$carreau_info <- renderUI(HTML("<i>Aucun carreau ici</i>"))
      return()
    }
    # if several hits, take the first (or area-based best)
    idx <- hit_idx[1]
    selected <- carreaux[idx, , drop = FALSE]
    # prepare HTML info: show id and selected variable value + a few key fields if present
    var <- input$var_select
    value <- selected[[var]]
    # pick some informative columns if they exist
    extras <- c("ind", "men", "code_insee", ".__id__")
    extras <- extras[extras %in% names(selected)]
    info_lines <- c()
    info_lines <- c(info_lines, paste0("<b>Carreau id:</b> ", selected$.__id__))
    info_lines <- c(info_lines, paste0("<b>", var, ":</b> ", format(round(as.numeric(value), 2), nsmall = 0)))
    for (cname in extras) {
      if (cname %in% c(var, ".__id__")) next
      info_lines <- c(info_lines, paste0("<b>", cname, ":</b> ", as.character(selected[[cname]])))
    }
    html <- paste(info_lines, collapse = "<br/>")
    output$carreau_info <- renderUI(HTML(html))
  })
  
  # 7) Addresses (cluster)
  observeEvent(input$dept_select, {
    leafletProxy("map_leaflet") %>% clearGroup("Adresses")
    adresses_dept <- adresses_sf %>% filter(dept == input$dept_select)
    leafletProxy("map_leaflet") %>%
      addCircleMarkers(data = adresses_dept, lng = ~longitude, lat = ~latitude,
                       color = "green", radius = 4, fillOpacity = 0.8,
                       clusterOptions = markerClusterOptions(), group = "Adresses")
  })
}

shinyApp(ui = ui, server = server)




library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(aws.s3)
library(arrow)
library(stringr)

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

#---------------------------------------
#   CACHES POUR EVITER LES RE-LECTURES
#---------------------------------------
cache_bdv <- list()
cache_carreaux <- list()

ui <- fluidPage(
  titlePanel("Visualisation optimisée : BDV + Filosofi + adresses"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dept_select", "Choisir un département :", choices = departements),
      checkboxInput("show_carreaux", "Afficher les carreaux Filosofi", FALSE),
      uiOutput("var_select_ui")
    ),
    
    mainPanel(
      leafletOutput("map_leaflet", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  #-----------------------------
  # 1) Chargement unique adresses
  #-----------------------------
  
  adresses_coords <- aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "sources/table-adresses-reu.parquet",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  ) %>% 
    mutate(dept = str_sub(code_commune_ref, 1, 2))
  
  adresses_sf <- st_as_sf(adresses_coords,
                          coords = c("longitude", "latitude"),
                          crs = 4326,
                          remove = FALSE)
  
  #-----------------------------
  # 2) UI dynamique variables Filosofi
  #-----------------------------
  
  output$var_select_ui <- renderUI({
    req(input$dept_select, input$show_carreaux)
    
    carreaux_file <- paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_",
                            input$dept_select, ".gpkg")
    
    if (!file.exists(carreaux_file)) return()
    
    # Lecture en cache
    if (is.null(cache_carreaux[[input$dept_select]])) {
      
      c_temp <- st_read(carreaux_file, quiet = TRUE)
      
      # ➜ Correction : transformation en WGS84 AVANT mise en cache
      c_temp <- st_transform(c_temp, 4326)
      
      cache_carreaux[[input$dept_select]] <<- c_temp
    }
    
    vars <- names(cache_carreaux[[input$dept_select]])
    
    selectInput("var_select", "Variable pour le dégradé :", choices = vars)
  })
  
  #-----------------------------
  # 3) Carte initiale
  #-----------------------------
  
  output$map_leaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2, lat = 46.5, zoom = 6)
  })
  
  #-----------------------------
  # 4) BDV optimisés (cache)
  #-----------------------------
  
  observeEvent(input$dept_select, {
    
    bdv_file <- paste0("R/0-brouillons/romain/bdv_departements/bdv_dep_",
                       input$dept_select, ".gpkg")
    
    if (!file.exists(bdv_file)) return()
    
    if (is.null(cache_bdv[[input$dept_select]])) {
      bdv <- st_read(bdv_file, quiet = TRUE)
      bdv <- st_transform(bdv, 4326)
      cache_bdv[[input$dept_select]] <<- bdv
    }
    
    bdv <- cache_bdv[[input$dept_select]]
    
    leafletProxy("map_leaflet") %>%
      clearGroup("BDV") %>%
      addPolylines(
        data = bdv,
        color = "blue", weight = 2,
        fillColor = "lightblue",
        fillOpacity = 0.4,
        group = "BDV"
      ) %>%
      fitBounds(st_bbox(bdv)[1], st_bbox(bdv)[2],
                st_bbox(bdv)[3], st_bbox(bdv)[4])
  })
  
  #-----------------------------
  # 5) Carreaux optimisés (cache + simplify)
  #-----------------------------
  observeEvent(c(input$show_carreaux, input$var_select), {
    
    leafletProxy("map_leaflet") %>% clearGroup("Carreaux")
    if (!input$show_carreaux) return()
    
    carreaux <- cache_carreaux[[input$dept_select]]
    if (is.null(carreaux)) return()
    
    var <- input$var_select
    req(var)
    
    v <- carreaux[[var]]
    if (!is.numeric(v) || all(is.na(v))) {
      showNotification("Variable non numérique ou vide", type = "error")
      return()
    }
    
    pal <- colorNumeric("Blues", v[!is.na(v)], na.color = "transparent")
    
    leafletProxy("map_leaflet") %>%
      addPolygons(
        data = carreaux,
        color = "black", weight = 0.4,
        fillColor = ~pal(v),
        fillOpacity = 0.7,
        group = "Carreaux"
      )
  })
  
  #-----------------------------
  # 6) Adresses (cluster)
  #-----------------------------
  
  observeEvent(input$dept_select, {
    
    leafletProxy("map_leaflet") %>% clearGroup("Adresses")
    
    adresses_dept <- adresses_sf %>% filter(dept == input$dept_select)
    
    leafletProxy("map_leaflet") %>%
      addCircleMarkers(
        data = adresses_dept,
        lng = ~longitude, lat = ~latitude,
        color = "green",
        radius = 4,
        fillOpacity = 0.8,
        clusterOptions = markerClusterOptions(),  # ⭐ accélération énorme
        group = "Adresses"
      )
  })
  
}

shinyApp(ui = ui, server = server)

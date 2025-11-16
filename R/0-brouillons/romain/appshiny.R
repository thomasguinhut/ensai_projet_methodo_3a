library(shiny)

departements <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                  "11", "12", "13", "14", "15", "16", "17", "18", "19", "21",
                  "22", "23", "24", "25", "26", "27", "28", "29", "2A", "2B",
                  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
                  "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
                  "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
                  "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
                  "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
                  "90", "91", "92", "93", "94", "95")

ui <- fluidPage(
  titlePanel("Visualisation des bureaux de vote et carreaux Filosofi"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dept_select", "Choisir un département :", 
                  choices = departements),
      checkboxInput("show_carreaux", "Afficher les carreaux Filosofi", FALSE),
      uiOutput("var_select_ui")
    ),
    
    mainPanel(
      leafletOutput("map_leaflet", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Menu déroulant dynamique pour la variable
  output$var_select_ui <- renderUI({
    req(input$dept_select, input$show_carreaux)
    carreaux_file <- paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_", 
                            input$dept_select, ".gpkg")
    if (!file.exists(carreaux_file)) return()
    carreaux <- st_read(carreaux_file, quiet = TRUE)
    vars <- names(carreaux)
    selectInput("var_select", "Variable pour le dégradé :", choices = vars)
  })
  
  # Carte vide initiale
  output$map_leaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2, lat = 46.5, zoom = 6)
  })
  
  # Charger les bureaux de vote quand un département est choisi
  observeEvent(input$dept_select, {
    bdv_file <- paste0("R/0-brouillons/romain/bdv_departements/bdv_dep_", 
                       input$dept_select, ".gpkg")
    if (!file.exists(bdv_file)) return()
    bdv <- st_read(bdv_file, quiet = TRUE)
    bdv_wgs <- st_transform(bdv, 4326)
    
    popup_bdv <- paste0("<b>Bureau : </b>", bdv_wgs$nom_bureau)
    
    leafletProxy("map_leaflet") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolylines(
        data = bdv_wgs,
        color = "blue", weight = 2,
        fillColor = "lightblue", fillOpacity = 0.4,
        popup = popup_bdv,
        group = "BDV"
      ) %>%
      fitBounds(
        lng1 = st_bbox(bdv_wgs)$xmin,
        lat1 = st_bbox(bdv_wgs)$ymin,
        lng2 = st_bbox(bdv_wgs)$xmax,
        lat2 = st_bbox(bdv_wgs)$ymax
      )
    
    # Mise à jour des carreaux si déjà cochés
    if (input$show_carreaux) {
      session$sendCustomMessage("reloadCarreaux", TRUE)
    }
  })
  
  observeEvent(c(input$show_carreaux, input$var_select), {
    leafletProxy("map_leaflet") %>% clearGroup("Carreaux")
    
    if (!input$show_carreaux) return()
    
    carreaux_file <- paste0("R/0-brouillons/romain/filosofi_departements/carreaux_dep_", input$dept_select, ".gpkg")
    if (!file.exists(carreaux_file)) return()
    carreaux <- st_read(carreaux_file, quiet = TRUE)
    carreaux_wgs <- st_transform(carreaux, 4326)
    
    # Vérifie que la variable sélectionnée existe et est numérique
    if (is.null(input$var_select) || !(input$var_select %in% names(carreaux_wgs))) {
      return()
    }
    
    # Vérifie que la colonne est numérique
    if (!is.numeric(carreaux_wgs[[input$var_select]])) {
      shiny::showNotification(
        "La variable sélectionnée n'est pas numérique. Veuillez en choisir une autre.",
        type = "error"
      )
      return()
    }
    
    # Filtre les NA pour éviter les erreurs
    values <- carreaux_wgs[[input$var_select]]
    values <- values[!is.na(values)]
    
    # Crée une palette de couleurs avec une plage valide
    pal <- colorNumeric(
      palette = "Blues",
      domain = values,
      na.color = "transparent"
    )
    
    # Popup avec la variable sélectionnée
    popup_carreaux <- paste0(
      "<b>Individus :</b> ", carreaux_wgs$ind, "<br>",
      "<b>Ménages :</b> ", carreaux_wgs$men, "<br>",
      "<b>", input$var_select, ":</b> ", carreaux_wgs[[input$var_select]]
    )
    
    leafletProxy("map_leaflet") %>%
      addPolygons(
        data = carreaux_wgs,
        color = "black", weight = 0.5,
        fillColor = ~pal(carreaux_wgs[[input$var_select]]),
        fillOpacity = 0.7,
        popup = popup_carreaux,
        group = "Carreaux"
      )
  })
  
}

shinyApp(ui = ui, server = server)

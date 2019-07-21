spatialUI <- function(id, label = "Spatial Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    box(
      title = "Bar Plot",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput(ns("bar"))
    ),
    box(
      title = "LeafLet Map",
      status = "primary",
      solidHeader = TRUE,
      leafletOutput(ns("mapplot")),
      absolutePanel(
        top = 60,
        right = 20,
        selectInput(
          ns("mapTexture"),
          "Map Texture",
          choices = list(
            "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
            "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
            "Stamen.Toner" = "Stamen.Toner",
            "CartoDB.Positron" = "CartoDB.Positron",
            "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
            "Stamen.Watercolor" = "Stamen.Watercolor",
            "Stamen.Terrain" = "Stamen.Terrain",
            "Esri.WorldImagery" = "Esri.WorldImagery",
            "Esri.WorldTerrain" = "Esri.WorldTerrain"
          ),
          selected = "CartoDB.Positron"
        ),
        selectInput(
          ns("mapColor"),
          "Points Color",
          choices = list(
            "Red" = 'red',
            "Green" = "green",
            "Blue" = "blue",
            "Black" = "black"
          )
        )
      ),
      verbatimTextOutput(ns("ass"))
  )))
}

spatialServer <- function(input, output, session, dataset) {
  
  output$bar <- renderPlotly({
    data <- as.data.frame(table(na.omit(dataset["identifiedBy"])))
    p <- plot_ly(
      data = data,
      source = "barsource",
      x = ~ Var1,
      y = ~ Freq
    )
    event_register(p, 'plotly_click')
    p
  })
  
  output$mapplot <- renderLeaflet({
    leaflet("mapplot", data = dataset) %>% addProviderTiles(input$mapTexture) %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  observeEvent(input$mapTexture, {
    if (length(dataset) == 0) {
      return(NULL)
    }
    leafletProxy("mapplot", data = dataset) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  observeEvent(input$mapColor, {
    if (length(dataset) == 0) {
      return(NULL)
    }
    leafletProxy("mapplot", data = dataset) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  observe({
    select <- event_data("plotly_click", source = "barsource")
    if(is.null(select)){
      leafletProxy("mapplot", data = dataset) %>%
        clearShapes() %>%
        addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    } else {
      dataset <- read.csv("www/csv/hyenaData.csv")
      newData <- dataset %>% filter(identifiedBy %in% select$x)
      leafletProxy("mapplot", data = newData) %>%
        clearShapes() %>%
        addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    }
  })
  
  output$ass <- renderPrint({a <-event_data("plotly_click", source = "barsource")
  a$x})
  
}

# Module UI

#' @title   mod_spatial_ui and mod_spatial_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_spatial
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(fluidRow(column(4, plotlyOutput(ns("countryBar"), height = "360px")),
                     column(4, verbatimTextOutput(ns("temp")))),
    fluidRow(column(
    12,
    leafletOutput(ns("mymap"), height = "240px"),
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
        selected = "Stamen.Toner"
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
    )
  ))
  )
}

# Module Server

#' @rdname mod_spatial
#' @export
#' @keywords internal

mod_spatial_server <- function(input, output, session, data) {
  ns <- session$ns
  
  output$countryBar <- renderPlotly({
    country <- data.frame(table(na.omit(data()["countryCode"])))%>%dplyr::rename(
      CountryName = Var1,
      NumberOfRecords = Freq)
    plot_ly(data = country, source = "barCountrt",
            x = ~CountryName,
            y = ~NumberOfRecords,
            name = "Countries",
            type = "bar"
    ) %>% layout(showlegend = FALSE, height = 320)
  })
  
  observe({
    click <- event_data("plotly_click", source = "barCountrt")
    if(is.null(click)){
      output$mymap <- renderLeaflet({
        leaflet(data = data()) %>%
          addProviderTiles(input$mapTexture) %>%
          addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
      })
      output$temp <- renderText("as")
    } else {
      new <- data() %>% filter(countryCode %in% click$x)
      leafletProxy("mymap", data = new) %>% clearShapes() %>%
        addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    }
  })
  
  observe({
    click <- event_data("plotly_selected", source = "barCountrt")
    if(is.null(click)){
      output$mymap <- renderLeaflet({
        leaflet(data = data()) %>%
          addProviderTiles(input$mapTexture) %>%
          addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
      })
      output$temp <- renderText("as")
    } else {
      new <- data() %>% filter(countryCode %in% click$x)
      leafletProxy("mymap", data = new) %>% clearShapes() %>%
        addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    }
  })
}


## To be copied in the UI
# mod_spatial_ui("spatial_ui_1")

## To be copied in the server
# callModule(mod_spatial_server, "spatial_ui_1")

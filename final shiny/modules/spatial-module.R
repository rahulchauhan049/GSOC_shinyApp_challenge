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
      )
    ),
    box(
      title = "LeafLet Map",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput(ns("barwithtime1")),
      uiOutput(ns("back1"))
    ),
    box(
      title = "LeafLet Map",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput(ns("time"))
    )
  ))
}

spatialServer <- function(input, output, session, dataset) {
  ns <- session$ns
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
    if (is.null(select)) {
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
  
  output$ass <-
    renderPrint({
      a <- event_data("plotly_click", source = "barsource")
      a$x
    })
  
  
  
  
  #For Drill down bar Chart with time data......................
  dataForBar <- format_bdvis(dataset, source = 'rgbif')
  
  
  names(dataForBar) = gsub("\\.", "_", names(dataForBar))
  if ("Date_collected" %in% colnames(dataForBar)) {
    if (length(which(!is.na(dataForBar$Date_collected))) == 0) {
      stop("Date_collected has no data")
    }
    dayofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected, na.rm =
                                              T), format = "%d"))
    weekofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected, na.rm =
                                               T), format = "%U"))
    monthofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected, na.rm =
                                                T), format = "%m"))
    Year_ = as.numeric(strftime(as.Date(dataForBar$Date_collected, na.rm =
                                          T), format = "%Y"))
    
  } else {
    stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
  }
  dataForBar <-
    cbind(dataForBar[c("genus", "species")], dayofYear, weekofYear, monthofYear, Year_)
  dataForBar <- arrange(dataForBar, as.numeric(dataForBar$monthofYear))
  dataForBar <- dataForBar[c("genus", "species", "monthofYear")]
  dataForBar <- na.omit(dataForBar)
  categoriesbar <- unique(dataForBar$genus)
  #...............................................................................
  
  
  
  #Bar Chart with time data.................................................
  current_categorybar <- reactiveVal()
  
  # report sales by category, unless a category is chosen
  mammals_data <- reactive({
    if (!length(current_categorybar())) {
      return(count(dataForBar, genus))
    }
    dataForBar %>%
      filter(genus %in% current_categorybar()) %>%
      count(species)
  })
  
  # the pie chart
  output$barwithtime1 <- renderPlotly({
    d <- setNames(mammals_data(), c("Names", "value"))
    
    plot_ly(d, source = "barwithtime") %>%
      add_bars(x = ~ Names,
               y = ~ value,
               color = ~ Names) %>%
      layout(title = current_categorybar() %||% "Total Sales")
  })
  
  # same as sales_data
  mammals_data_time <- reactive({
    if (!length(current_categorybar())) {
      return(count(dataForBar, genus, monthofYear))
    }
    dataForBar %>%
      filter(genus %in% current_categorybar()) %>%
      count(species, monthofYear)
  })
  
  output$time <- renderPlotly({
    d <- setNames(mammals_data_time(), c("color", "month", "value"))
    plot_ly(d) %>%
      add_lines(x = ~ month,
                y = ~ value,
                color = ~ color)
  })
  
  # update the current category if the clicked value matches a category
  observe({
    cd <- event_data("plotly_click", source = "barwithtime")$x
    if (isTRUE(cd %in% categoriesbar))
      current_categorybar(cd)
  })
  
  # populate back button if category is chosen
  output$back1 <- renderUI({
    if (length(current_categorybar()))
      actionButton(ns("clear"), "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, current_categorybar(NULL))
  
  #Bar chart End here.....................................
  
}

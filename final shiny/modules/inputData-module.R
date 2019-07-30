inputTabUi <- function(id) {
  ns <- NS(id)
  
  fluidRow(div(
    # -------------------------------
    tagList(
      column(
        12,
        
        column(
          3,
          h1("Add Occurrence Data"),
          tabsetPanel(
            type = "tabs",
            tabPanel("Existing Data",
                     selectizeInput(ns("dataSet"),
                                    "Select Sample Datasets",
                                    choices = c( "Mammals"="smallData.csv", "Hyena"="hyenaData.csv"),
                                    selected = "Hyena"
                     ),
                     actionButton(ns("loadexisting"), "Load New Dataset")
            ),
            # ------------- DB Module -------------------
            tabPanel(
              "Download Data",
              div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
              textInput(
                ns("scientificName"),
                label = h3("Scientific Name:"),
                value = "Puma concolor"
              ),
              
              numericInput(
                ns("recordSize"),
                label = h3("Record Size:"),
                value = 500
              ),
              
              selectInput(
                ns("hasCoords"),
                label = h3("Records Filter:"),
                choices = list(
                  "With Coordinates" = "1",
                  "Without Coordinates" = "2",
                  "No Filter" = "3"
                ),
                selected = 3
              ),
              
              radioButtons(
                ns("queryDB"),
                label = h3("Online Database:"),
                choices = list(
                  "GBIF (Global Biodiversity Information Facility)" = "gbif",
                  "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
                  "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
                  "Vertnet (Vertebrate Network)" = "vertnet",
                  "BISON (Biodiversity Information Serving Our Nation)" = "bison",
                  "iNaturalist" = "inat",
                  "ALA (Atlas of Living Australia)" = "ala"
                  # "OBIS (Ocean Biogeographic Information System)" = "obis",
                  # "AntWeb" = "antweb"
                ),
                selected = "gbif"
              ),
              
              br(),
              div(
                id = "queryDatabaseDiv",
                class = "activeButton",
                actionButton(ns("queryDatabase"), "Query Database", icon("download"))
              ),
              br()
            ),
            
            # ------------- End of DB Module -------------------
            
            # ------------- Local Disk Module -------------------
            tabPanel(
              "Upload Data",
              div(class = "secondaryHeaders", h3("Option 02: From Local Disk")),
              div(
                id = "inputFileDiv",
                class = "activeButton",
                fileInput(
                  ns("inputFile"),
                  label = h3("CSV / DWCA ZIP file input"),
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".zip",
                    "application/zip"
                  )
                )
              )
            ),
            # checkboxInput(ns("darwinizerControl"),
            #               label = "Perform Header Cleaning",
            #               value = TRUE),
            
            helpText(
              "To manually edit or clean headers, use ",
              a("bdDwC", href = "https://github.com/bd-R/bdDwC"),
              " package."
            )
            
            
            # ------------- End of Local Disk Module -------------------
            
            
          )
          
          
        ),column(9,
                 div(
                   class = "center",
                   fluidRow(
                     infoBox("# of Records", textOutput(ns("inputDataRows")), icon = icon("list-ol")),
                     infoBox(
                       "# of Fields",
                       textOutput(ns("inputDataColumns")),
                       icon = icon("th-list"),
                       color = "purple"
                     ),
                     infoBox(
                       "# of Scientific Names",
                       textOutput(ns("inputDataSpecies")),
                       icon = icon("paw"),
                       color = "yellow"
                     )
                   ))),
        
        # ------------- Map / Table Module -------------------
        column(6,
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Map View",
                   leafletOutput(ns("mymap"), height = "400"),
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
                   DT::dataTableOutput(ns("inputDataTable"))
                 )
               )),
        column(3,
               "#Missing Records", verbatimTextOutput(ns("missing")),
               "#Latitude", verbatimTextOutput(ns("latitude")),
               "#Longitude", verbatimTextOutput(ns("longitude")),
               "#No. of Countries", verbatimTextOutput(ns("country")),
               "#No. of Order", verbatimTextOutput(ns("order")),
               "#No. of Family", verbatimTextOutput(ns("family")),
               "#No. of Genus", verbatimTextOutput(ns("genus")),
               "#No. of Species", verbatimTextOutput(ns("species"))
             
               )
        
        # ------------- End of Map/Table Module -------------------
      )
    )
    
    
    # -------------------------------
    
    
  ))
}

inputTabServer <- function(input, output, session) {
  #Initially loaded data
  returnData <- read.csv("www/csv/hyenaData.csv")
  map <- leafletProxy("mymap")
  
  #when user click on button called load existing dataset
  observeEvent(input$loadexisting, {
    returnData <<- read.csv(paste("www/csv/",input$dataSet, sep = ""))
    dataLoadedTask(returnData)
  })
  
  #when user click on button called Query Database
  observeEvent(input$queryDatabase, {
    withProgress(message = paste("Querying", input$queryDB, "..."), {
      if (input$queryDB == "gbif") {
        data <-
          rgbif::occ_search(
            scientificName = input$scientificName,
            limit = input$recordSize,
            hasCoordinate = switch(
              input$hasCoords,
              "1" = TRUE,
              "2" = FALSE,
              "3" = NULL
            )
          )
        returnData <<- data$data
        
      } else {
        warnings <- capture.output(
          data <-
            spocc::occ(
              query = input$scientificName,
              from = input$queryDB,
              limit = input$recordSize,
              has_coords = switch(
                input$hasCoords,
                "1" = TRUE,
                "2" = FALSE,
                "3" = NULL
              )
            ),
          type = "message"
        )
        
        if (length(warnings) > 0) {
          showNotification(paste(warnings, collapse = " "),
                           duration = 6)
        }
        
        tempData <- data[[input$queryDB]]$data[[1]]
        returnData <<- tempData
      }
    })
    
    dataLoadedTask(returnData)
    
    
  })
  
  #When user click on upload dataset
  observeEvent(input$inputFile, {
    withProgress(message = paste("Reading", input$inputFile$name, "..."), {
      
      if (is.null(input$inputFile))
        return("No data to view")
      
      if (grepl("zip", tolower(input$inputFile$type))) {
        message("Reading DWCA ZIP...")
        finchRead <-
          finch::dwca_read(input$inputFile$datapath, read = T)
        returnData <<- finchRead$data[[1]]
        
      } else {
        returnData <<-
          data.table::fread(input$inputFile$datapath)
      }
    })
    dataLoadedTask(returnData)
    
    
  })
  
  #when user change design of map
  observeEvent(input$mapTexture, {
    if (length(returnData) == 0) {
      return(NULL)
    }
    leafletProxy("mymap", data = returnData) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  #when user change color of map points
  observeEvent(input$mapColor, {
    if (length(returnData) == 0) {
      return(NULL)
    }
    leafletProxy("mymap", data = returnData) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  #Load map with initially loaded data
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(input$mapTexture) %>%
      setView(0, 0, zoom = 2)
  })
  
  #Load Table with initial Dataset
  output$inputDataTable <- DT::renderDataTable(DT::datatable({
    tableData <- returnData[c("identifiedBy", "scientificName", "family", "genus")]
    tableData <- na.omit(tableData)
    if (!nrow(tableData[-which(tableData[, "identifiedBy"] == ""), ]) == 0) {
      tableData <- tableData[-which(tableData[, "identifiedBy"] == ""), ]
    }
    head(tableData)
  }, options = list(scrollX = TRUE)))
  
  
  #----------------Info About Data------------------------------------------
  output$inputDataRows <- renderText(nrow(returnData))
  output$inputDataColumns <- renderText(length(returnData))
  output$inputDataSpecies <-
    renderText(nrow(unique(returnData["scientificName"])))
  
  output$missing <- renderPrint({
    sum(is.na(returnData)) 
  })
  
  output$latitude <- renderPrint({
    latitude <- na.omit(returnData["decimalLatitude"])
    latitude <-nrow(latitude)
    return(latitude)
  })
  
  output$longitude <- renderPrint({
    longitude <- na.omit(returnData["decimalLongitude"])
    longitude <-nrow(longitude)
    return(longitude)
  })
  
  output$country <- renderPrint(nrow(unique(na.omit(returnData["countryCode"]))))
  
  output$order <- renderPrint({nrow(unique(na.omit(returnData["order"])))})
  output$family <- renderPrint({nrow(unique(na.omit(returnData["family"])))})
  output$genus <- renderPrint({nrow(unique(na.omit(returnData["genus"])))})
  output$species <- renderPrint({nrow(unique(na.omit(returnData["species"])))})
  
  #-----------------------Info of data ends here--------------------------------
  
  
  #This function run when user change dataset-------------------------------
  dataLoadedTask <- function(data) {
    if (length(data) == 0) {
      showNotification("Empty data returned! Try different setting.",
                       duration = 2)
      return()
    }
    
    output$inputDataRows <- renderText(nrow(returnData))
    output$inputDataColumns <- renderText(length(returnData))
    output$inputDataSpecies <-
      renderText(nrow(unique(returnData["scientificName"])))
    output$missing <- renderPrint({
      sum(is.na(returnData)) 
    })
    
    output$latitude <- renderPrint({
      latitude <- na.omit(returnData["decimalLatitude"])
      latitude <-nrow(latitude)
      return(latitude)
    })
    
    output$longitude <- renderPrint({
      longitude <- na.omit(returnData["decimalLongitude"])
      longitude <-nrow(longitude)
      return(longitude)
    })
    
    output$country <- renderPrint({nrow(unique(na.omit(returnData["countryCode"])))})
    output$order <- renderPrint({nrow(unique(na.omit(returnData["order"])))})
    output$family <- renderPrint({nrow(unique(na.omit(returnData["family"])))})
    output$genus <- renderPrint({nrow(unique(na.omit(returnData["genus"])))})
    output$species <- renderPrint({nrow(unique(na.omit(returnData["species"])))})
    

    
    if ("decimalLatitude" %in% colnames(returnData)) {
      returnData$decimalLatitude <<-
        as.numeric(returnData$decimalLatitude)
      returnData$decimalLongitude <<-
        as.numeric(returnData$decimalLongitude)
    }
    
    # ------------ End of Darwinizing Data -------------
    
    try(leafletProxy("mymap", data = returnData) %>%
          clearShapes() %>%
          addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor))
    
    output$inputDataTable <- DT::renderDataTable(DT::datatable({
      tableData <- returnData[c("identifiedBy", "scientificName", "family", "genus")]
      tableData <- na.omit(tableData)
      if (!nrow(tableData[-which(tableData[, "identifiedBy"] == ""), ]) == 0) {
        tableData <- tableData[-which(tableData[, "identifiedBy"] == ""), ]
      }
      head(tableData)
    }, options = list(scrollX = TRUE)))
    
    
    shinyjs::runjs(code = paste('$("#', "queryDatabaseDiv", '").addClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', "queryDatabaseDiv", '").removeClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', "inputFileDiv", '").addClass("readyButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', "inputFileDiv", '").removeClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', "dataToConfigureDiv", '").addClass("completedButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', "dataToConfigureDiv", '").removeClass("activeButton");', sep = ""))
    
    
    showNotification("Read Data Successfully", duration = 2)
    
    
    # --------- Setting flag tab statistic boxes -------
    # TODO
    
  }
  
  
  returnDataReact <- reactive({
    # Input actions that need to trigger new dataframe return
    input$dataset
    input$inputFile
    input$queryDatabase
    
    returnData
  })
  #Return Dataset so it can be used by other tabs
  return(returnDataReact)
}
options(shiny.maxRequestSize = 50 * 1024 ^ 2)
library(bdchecks)

returnData <- read.csv("smallData.csv")

shinyServer(function(input, output, session) {
  
  
  
   map <- leafletProxy("mymap")
  
   observeEvent(input$loadexisting, {
     returnData <<- read.csv(paste("www/csv/",input$dataSet, sep = ""))
     dataLoadedTask(returnData)
   })
  
  
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
  
  
  observeEvent(input$mapTexture, {
    if (length(returnData) == 0) {
      return(NULL)
    }
    leafletProxy("mymap", data = returnData) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  observeEvent(input$mapColor, {
    if (length(returnData) == 0) {
      return(NULL)
    }
    leafletProxy("mymap", data = returnData) %>%
      clearShapes() %>%
      addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(input$mapTexture) %>%
      setView(0, 0, zoom = 2)
  })
  
  dataLoadedTask <- function(data) {
    if (length(data) == 0) {
      showNotification("Empty data returned! Try different setting.",
                       duration = 2)
      return()
    }
    
    # ------------ Darwinizing Data -------------
    
    # if (input$darwinizerControl) {
    #     showNotification("Cleaning Headers", duration = 2)
    #     dictionaryPath <-
    #         system.file("txts/customDwCdictionary.txt", package = "bdclean")
    #     customDictionary <-
    #         data.table::fread(file = dictionaryPath)
    #
    #     darwinizer <-
    #         bdDwC::darwinize_names(as.data.frame(returnData), as.data.frame(customDictionary))
    #
    #     fixed <-
    #         darwinizer[darwinizer$matchType == "Darwinized",]
    #
    #     if (nrow(fixed) > 0) {
    #         tidyData <- bdDwC::renameUserData(returnData, darwinizer)
    #
    #         returnData <<- tidyData
    #
    #         showNotification(paste(
    #             "Converted Columns:",
    #             paste(
    #                 paste(fixed[, 1], collapse = ", "),
    #                 paste(fixed[, 2], collapse = ", "),
    #                 sep = " -> "
    #             )
    #         ),
    #         duration = 7)
    #     }
    # }
    
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
      returnData
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

  
  # --------- Plotting taxomic plots -------

 callModule(taxonomicTabServer, "taxo", returnData)
  
  #----------End of Taxonomic Tab-----------
  
  
  
  # --------- Plotting spatial plots -------
  
  callModule(spatialServer, "spatial", returnData)
  
  #----------End of spatial Tab-----------
  
  
  # --------- Plotting Temporal plots -------
  
  callModule(temporalServer, "temporal", returnData)
  
  #----------End of Temporal Tab-----------
  
  
})# Server Ends Here
#Importing packages
library("rgbif")
library("bdvis")
library("rinat")
library("shinythemes")
library(shiny)
library("crosstalk")
library(dplyr)

#import Datasets
a <- read.csv("a.csv")
country <- read.csv("countrycode.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    df <- reactive({
        tryCatch({
        df <- read.csv(
            paste("www/csv/", input$datasetselected, sep = ""),
            header = input$header1,
            sep = input$sep1,
            quote = input$quote1
        ) %>% 
            rename(
                 latitude = decimalLatitude,
                 longitude = decimalLongitude
            )
        
    },
    error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
    })
        
    })
 
    
    output$tableoutput <- DT::renderDataTable({df<-df()
    if (input$disp1 == "head") {
        return(head(df))
    }
    else {
        return(df)
    }
    })
    output$tableupload <- DT::renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch({
            df <- read.csv(
                input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote
            )
            df<-na.omit(df)
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
        if (input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    
    #For select all attritbute in gbif data
    observe({
        if ("SelectAll" %in% input$fields)
            selected_choices = a[-1, ] # choose all the choices _except_ "Select All"
        else
            selected_choices = input$fields # update the select input with choice selected by user
        updateSelectInput(session, "fields", selected = selected_choices)
    })
    
    #Download gbif type of data
    fileext <- reactive({
        switch(
            input$dataradio,
            "Excel (CSV)" = "csv",
            "Text (TSV)" = "txt",
            "Text (Space Separated)" = "txt",
            "Doc" = "doc"
        )
        
    })
    #Download rinat type of data
    fileextrinat <- reactive({
        switch(
            input$dataradiorinat,
            "Excel (CSV)" = "csv",
            "Text (TSV)" = "txt",
            "Text (Space Separated)" = "txt",
            "Doc" = "doc"
        )
        
    })
    #Download button for Data
    output$databutton <- downloadHandler(
        filename = function() {
            paste(input$dataradio, fileext(), sep = ".")
        },
        content = function(file) {
            sep <-
                switch(
                    input$dataradio,
                    "Excel (CSV)" = ",",
                    "Text (TSV)" = "\t",
                    "Text (Space Separated)" = " ",
                    "Doc" = " "
                )
            write.table(
                occ <-
                    gbif(input$sname, input$limit, input$cntry, input$fields),
                file,
                sep = sep,
                row.names = FALSE
            )
        }
    )
    
    
    #Download button for rinat
    output$databuttonrinat <- downloadHandler(
        filename = function() {
            paste(input$dataradiorinat, fileextrinat(), sep = ".")
        },
        content = function(file) {
            sep <-
                switch(
                    input$dataradiorinat,
                    "Excel (CSV)" = ",",
                    "Text (TSV)" = "\t",
                    "Text (Space Separated)" = " ",
                    "Doc" = " "
                )
            write.table(
                inat <-
                    inatdata(
                        input$queryrinat,
                        input$taxon_namerinat,
                        input$maxrinat,
                        input$year,
                        input$month,
                        input$date
                    ),
                file,
                sep = sep,
                row.names = FALSE
            )
        }
    )
    observeEvent(input$searchrinat, {
        output$rinattable <-
            renderDataTable(
                inat <-
                    inatdata(
                        input$queryrinat,
                        input$taxon_namerinat,
                        input$maxrinat,
                        input$year,
                        input$month,
                        input$date
                    )
            )
    })
    
    observeEvent(input$search, {
        output$table = renderDataTable(occ <-
                                           gbif(input$sname, input$limit, input$cntry, input$fields))
    })
    
    ###########################################
    shared_data <- SharedData$new(df)
    
    output$mymap <- renderLeaflet({

        m <- leaflet(shared_data) %>%
            addTiles() %>% addMarkers() # Add default OpenStreetMap map tiles
        m
    })
    output$v <- renderText(class( shared_data))
    output$tb <- DT::renderDataTable({
        shared_data
    }, server = FALSE)
})#END OF SERVER

#Data Download...........................................................................

#rinat...................................................................................
inatdata <-
    function(queries = NULL,
             taxon = "reptileindia",
             max = 50,
             year = NULL,
             month = NULL,
             day = NULL) {
        inat <-
            get_inat_obs(
                query = queries,
                taxon_name = taxon,
                taxon_id = NULL,
                quality = NULL,
                geo = TRUE,
                year = year,
                month = month,
                day = day,
                bounds = NULL,
                maxresults = max,
                meta = FALSE
            )
    }

#gbif....................................................................................
gbif <-
    function(sname = "Mammalia",
             olimit = 10,
             cntry = "world",
             fields) {
        if (cntry == "world")
            cntry = NULL
        key <- name_backbone(name = sname)$usageKey
        occ <-
            occ_search(
                taxonKey = key,
                country = cntry,
                limit = olimit,
                year = "2017,2018" ,
                return = "data"
            )
        if (is.null(fields)) {
            fields = c("class", "family", "genus", "species")
        }
        else{
            fields = fields
        }
        return(occ[fields])
    }


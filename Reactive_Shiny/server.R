#Importing packages
# library("rgbif")
# library("bdvis")
# library("rinat")
# library("shinythemes")
# library(shiny)
# library("crosstalk")
# library(dplyr)
# library("plotly")
# library(r2d3)
# library("collapsibleTree")

options(shiny.maxRequestSize=30*1024^2)
#import Datasets
columnName <- read.csv("www/csv/columnNames.csv")
country <- read.csv("www/csv/countrycode.csv")

shinyServer(function(input, output, session) {
    df <- reactive({
        if (input$datatabs == 1) {
            tryCatch({
                df <- read.csv(
                    paste("www/csv/", input$datasetselected, sep = ""),
                    header = input$header1,
                    sep = input$sep1,
                    quote = input$quote1
                )  %>%
                    rename(latitude = decimalLatitude,
                           longitude = decimalLongitude)
                df <- df[!is.na(df$latitude), ]
                df <- df[!is.na(df$longitude), ]
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            })
        } else if (input$datatabs == 2) {
            req(input$file1)
            
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch({
                df <- read.csv(
                    input$file1$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote
                )%>%
                    rename(latitude = decimalLatitude,
                           longitude = decimalLongitude)
                df <- df[!is.na(df$latitude), ]
                df <- df[!is.na(df$longitude), ]
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            })
        }
    })
    
    
    output$tableoutput <- DT::renderDataTable({
        df <- df()
        if (input$disp1 == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    output$tableupload <- DT::renderDataTable({
        df <- df()
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
            selected_choices = columnName[-1,] # choose all the choices _except_ "Select All"
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
        if(nrow(df())>1800){
        leaflet(shared_data, options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(
                updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                updateWhenIdle = TRUE           # map won't load new tiles when panning
            )) %>% addCircles(weight = 0) # Add default OpenStreetMap map tiles
        }else {
            leaflet(shared_data, options = leafletOptions(preferCanvas = TRUE)) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(
                    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                    updateWhenIdle = TRUE           # map won't load new tiles when panning
                )) %>% addMarkers() # Add default OpenStreetMap map tiles
        }
    })
    output$v <- renderText(class(shared_data))
    output$tb <- DT::renderDataTable(
        shared_data,
        options = list(
            pageLength = 4,
            scrollX = '400px',
            scrollY = "200px"
        ),
        server = FALSE
    )
    
    #Bar Plot
    output$bar <- renderPlotly({
        if (input$barselect == "Kingdom") {
            label <- ~ kingdom
        } else if (input$barselect == "Phylum") {
            label <- ~ phylum
        } else if (input$barselect == "Family") {
            label <- ~ family
        } else if (input$barselect == "Genus") {
            label <- ~ genus
        } else if (input$barselect == "Species") {
            label <- ~ species
        } else{
            label <- ~ order
        }
        plot_ly(data = shared_data,
                x = label,
                color = label)
    })
    
    #Pie Chart
    output$pie <- renderPlotly({
        if (input$pieselect == "Kingdom") {
            label <- ~ kingdom
        } else if (input$pieselect == "Phylum") {
            label <- ~ phylum
        } else if (input$pieselect == "Family") {
            label <- ~ family
        } else if (input$pieselect == "Genus") {
            label <- ~ genus
        } else if (input$pieselect == "Species") {
            label <- ~ species
        } else{
            label <- ~ order
        }
        plot_ly(
            data = shared_data,
            labels = label,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text'
        ) %>%
            layout(
                title = paste(
                    'Quantity of perticular ',
                    input$pieselect,
                    ' in Biodiversity data.'
                ),
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
        
        
    })
    
    output$tree <- renderCollapsibleTree({
        data <- df()
        data <-
            na.omit(data[c("kingdom", "phylum", "order", "family", "genus")])
        data <- arrange(data, family)
        temp <- as.data.frame(table(data["genus"]))
        data <- unique(data)
        temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
        temp <-
            temp[c("phylum", "order", "family", "genus", "Freq")]
        
        temp %>%
            group_by(phylum, order, family, genus) %>%
            summarize(`Freq` = n()) %>%
            collapsibleTreeSummary(
                hierarchy = input$tree,
                root = "Geography",
                width = "100%",
                attribute = "Freq",
                zoomable = FALSE
            )
    })
    
    output$fastmap <- renderLeaflet({
        leaflet(df(), options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(
                updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                updateWhenIdle = TRUE           # map won't load new tiles when panning
            )) %>% addCircles(weight = 0) # Add default OpenStreetMap map tiles
        
    })
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

#Importing packages
# library("rgbif")
# library("bdvis")
# library("rinat")
# library("shinythemes")
# library(shiny)
# library("crosstalk")
# library(dplyr)
#library("plotly")
# library(r2d3)
# library("collapsibleTree")
# library(nycflights13)
# library(ggstat)
# library(htmlwidgets)


js <- c(
    "function(el, x){",
    "  el.on('plotly_legenddoubleclick', function(evtData) {",
    "    Shiny.setInputValue('trace', evtData.data[evtData.curveNumber].name);",
    "  });",
    "}")

flights <- flights
arr_time <- flights$arr_time
dep_time <- flights$dep_time
arr_bins <- bin_fixed(arr_time, bins = 150)
dep_bins <- bin_fixed(dep_time, bins = 150)
arr_stats <- compute_stat(arr_bins, arr_time)
dep_stats <- compute_stat(dep_bins, dep_time)

mam <- read.csv("www/csv/mammalsLarge.csv")
mammals <- read.csv("www/csv/hyenaData.csv")
hyena <- read.csv('www/csv/hyenaData.csv')

#for drill down pie chart....................
piedata <- na.omit(hyena[c("genus","species")])
categories <- unique(mammals$genus)
#.............................................


#For Drill down bar Chart with time data......................
dataForBar <- format_bdvis(hyena,source='rgbif')


names(dataForBar)=gsub("\\.","_",names(dataForBar))
if("Date_collected" %in% colnames(dataForBar)){
    if(length(which(!is.na(dataForBar$Date_collected)))==0){
        stop("Date_collected has no data")
    }
    dayofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected,na.rm=T), format = "%d"))
    weekofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected,na.rm=T), format = "%U"))
    monthofYear = as.numeric(strftime(as.Date(dataForBar$Date_collected,na.rm=T), format = "%m"))
    Year_ = as.numeric(strftime(as.Date(dataForBar$Date_collected,na.rm=T), format = "%Y"))
    
} else {
    stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
}
dataForBar <- cbind(dataForBar[c("genus", "species")],dayofYear,weekofYear,monthofYear,Year_)
dataForBar<-arrange(dataForBar,as.numeric(dataForBar$monthofYear))
dataForBar<- dataForBar[c("genus", "species", "monthofYear")]
dataForBar <- na.omit(dataForBar)
categoriesbar <- unique(dataForBar$genus)
#...............................................................................




options(shiny.maxRequestSize = 30 * 1024 ^ 2)
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
                df <- df[!is.na(df$latitude),]
                df <- df[!is.na(df$longitude),]
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
                ) %>%
                    rename(latitude = decimalLatitude,
                           longitude = decimalLongitude)
                df <- df[!is.na(df$latitude),]
                df <- df[!is.na(df$longitude),]
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
            selected_choices = columnName[-1, ] # choose all the choices _except_ "Select All"
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
        if (nrow(df()) > 1800) {
            leaflet(shared_data, options = leafletOptions(preferCanvas = TRUE)) %>%
                addProviderTiles(
                    providers$Esri.WorldGrayCanvas,
                    options = providerTileOptions(
                        updateWhenZooming = FALSE,
                        # map won't update tiles until zoom is done
                        updateWhenIdle = TRUE           # map won't load new tiles when panning
                    )
                ) %>% addCircles(weight = 0) # Add default OpenStreetMap map tiles
        } else {
            leaflet(shared_data, options = leafletOptions(preferCanvas = TRUE)) %>%
                addProviderTiles(
                    providers$Esri.WorldGrayCanvas,
                    options = providerTileOptions(
                        updateWhenZooming = FALSE,
                        # map won't update tiles until zoom is done
                        updateWhenIdle = TRUE           # map won't load new tiles when panning
                    )
                ) %>% addMarkers() # Add default OpenStreetMap map tiles
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
    
    #Page 4............................................................................
    timedata <- reactive({
        mammals <-
            read.csv(paste("www/csv/", input$temporaldataset, sep = ""))
        mammals <- format_bdvis(mammals, source = 'rgbif')
        
        
        names(mammals) = gsub("\\.", "_", names(mammals))
        if ("Date_collected" %in% colnames(mammals)) {
            if (length(which(!is.na(mammals$Date_collected))) == 0) {
                stop("Date_collected has no data")
            }
            dayofYear = as.numeric(strftime(
                as.Date(mammals$Date_collected, na.rm = T),
                format = "%d"
            ))
            weekofYear = as.numeric(strftime(
                as.Date(mammals$Date_collected, na.rm = T),
                format = "%U"
            ))
            monthofYear = as.numeric(strftime(
                as.Date(mammals$Date_collected, na.rm = T),
                format = "%m"
            ))
            Year_ = as.numeric(strftime(
                as.Date(mammals$Date_collected, na.rm = T),
                format = "%Y"
            ))
            
        } else {
            stop(
                "Date_collected not found in data. Please use format_bdvis() to fix the problem"
            )
        }
        a = cbind(mammals[c("order", "genus", "family", "species")], dayofYear, weekofYear, monthofYear, Year_)
        return(a)
    })
    
    
    
    output$timebars <- renderPlot({
        a <- timedata()
        a <- arrange(a, as.numeric(a$dayofYear))
        a <- a[c(input$temporalcolumn, "dayofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = dayofYear,
                value = Freq
            )
        
        ggplot(data = a, aes(
            x = variable,
            y = value,
            fill = group
        )) +
            geom_bar(stat = "identity") + xlab("Days") + ylab("Quantity")
    })
    
    output$timerose <- renderPlot({
        a <- timedata()
        a <- arrange(a, as.numeric(a$dayofYear))
        a <- a[c(input$temporalcolumn, "dayofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = dayofYear,
                value = Freq
            )
        
        ggplot(data = a, aes(
            x = variable,
            y = value,
            fill = group
        )) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("")
    })
    
    
    #Month data........................................
    output$monthrose <- renderPlot({
        a <- timedata()
        a <- arrange(a, as.numeric(a$monthofYear))
        a <- a[c(input$temporalcolumn, "monthofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = monthofYear,
                value = Freq
            )
        
        ggplot(data = a, aes(
            x = variable,
            y = group,
            fill = value
        )) +
            geom_tile(colour = "black", size = 0.1) +
            coord_polar() + xlab("") + ylab("")
        
    })
    
    output$monthtimebars <- renderPlot({
        a <- timedata()
        a <- arrange(a, as.numeric(a$monthofYear))
        a <- a[c(input$temporalcolumn, "monthofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = monthofYear,
                value = Freq
            )
        
        ggplot(data = a, aes(
            x = variable,
            y = value,
            fill = group
        )) +
            geom_bar(stat = "identity") + xlab("Days") + ylab("Quantity")
    })
    
    output$monthtimerose <- renderPlot({
        a <- timedata()
        a <- arrange(a, as.numeric(a$monthofYear))
        a <- a[c(input$temporalcolumn, "monthofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = monthofYear,
                value = Freq
            )
        
        ggplot(data = a, aes(
            x = variable,
            y = value,
            fill = group
        )) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("")
    })
    
    #year Plots..................................
    
    output$yearlines <- renderPlotly({
        a <- timedata()
        a <- arrange(a, as.numeric(a$Year_))
        a <- a[c(input$temporalcolumn, "Year_")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = Year_,
                value = Freq
            )
        
        plot_ly(
            a,
            x = ~ variable,
            y = ~ value,
            color = ~ group
        ) %>%
            add_lines()
    })
    
    output$yearlines3d <- renderPlotly({
        a <- timedata()
        a <- arrange(a, as.numeric(a$Year_))
        a <- a[c(input$temporalcolumn, "Year_")]
        a <-
            data.frame(table(a)) %>% rename(
                group = input$temporalcolumn,
                variable = Year_,
                value = Freq
            )
        plot_ly(
            a,
            x = ~ variable,
            y = ~ value,
            z = ~ group,
            type = 'scatter3d',
            mode = 'lines',
            color = ~ group
        )
    })
    
    #Practice for Reactive....................................
    fifthtime <- reactive({
        mammals <- read.csv("www/csv/hyenaData.csv")
        mammals <- format_bdvis(mammals,source='rgbif')
        
        
        names(mammals)=gsub("\\.","_",names(mammals))
        if("Date_collected" %in% colnames(mammals)){
            if(length(which(!is.na(mammals$Date_collected)))==0){
                stop("Date_collected has no data")
            }
            dayofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%d"))
            weekofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%U"))
            monthofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%m"))
            Year_ = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%Y"))
            
        } else {
            stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
        }
        a = cbind(mammals["genus"],dayofYear,weekofYear,monthofYear,Year_)
        return(a)
    })
    
    output$arr_time <- renderPlotly({
        plot_ly(arr_stats, source = "arr_time") %>%
            add_bars(x = ~ xmin_, y = ~ count_)
    })
    output$dep_time <- renderPlotly({
        plot_ly(dep_stats, source = "dep_time") %>%
            add_bars(x = ~ xmin_, y = ~ count_)
        
    })
    observe({
        brush <- event_data("plotly_brushing", source = "arr_time")
        p <- plotlyProxy("dep_time")
        if (is.null(brush)) {
            plotlyProxyInvoke(p, "restyle", "y", list(dep_stats$count_))
        } else {
            dep_time_filter <-
                dep_time[between(dep_time, brush$x[1], brush$x[2])]
            dep_count <-
                compute_stat(dep_bins, dep_time_filter)$count_
            plotlyProxyInvoke(p, "restyle", "y", list(dep_count))
        }
    })
    
    observe({
        brush <- event_data("plotly_brushing", source = "dep_time")
        p <- plotlyProxy("arr_time")
        if (is.null(brush)) {
            plotlyProxyInvoke(p, "restyle", "y", list(arr_stats$count_))
        } else {
            arr_time_filter <-
                arr_time[between(arr_time, brush$x[1], brush$x[2])]
            arr_count <-
                compute_stat(arr_bins, arr_time_filter)$count_
            plotlyProxyInvoke(p, "restyle", "y", list(arr_count))
        }
    })
    
    
    output$fifthpie <- renderPlotly({
        plot_ly(y = mam$family, source = "reactiveBars")
    })
    
    output$fifthmap <- renderLeaflet({
        leaflet(mam, options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles(
                providers$Esri.WorldGrayCanvas,
                options = providerTileOptions(
                    updateWhenZooming = FALSE,
                    # map won't update tiles until zoom is done
                    updateWhenIdle = TRUE           # map won't load new tiles when panning
                )
            ) %>% addMarkers( ~ decimalLongitude, ~ decimalLatitude) # Add default OpenStreetMap map tiles
        
    })
    
    
    
    
    observe({
        select <- event_data("plotly_selected", source = "reactiveBars")
        newData <- mam %>% filter(family %in% select$y)
        if (is.null(select)) {
            leafletProxy("fifthmap", data = mam) %>% clearMarkers() %>%
                addMarkers( ~ decimalLongitude, ~ decimalLatitude)
        } else{
            leafletProxy("fifthmap", data = newData) %>% clearMarkers() %>%
                addMarkers( ~ decimalLongitude, ~ decimalLatitude)
        }
    })
    
    observe({
        select <- event_data("plotly_click", source = "reactiveBars")
        newData <- mam %>% filter(family %in% select$y)
        if (is.null(select)) {
            leafletProxy("fifthmap", data = mam) %>% clearMarkers() %>%
                addMarkers( ~ decimalLongitude, ~ decimalLatitude)
        } else{
            leafletProxy("fifthmap", data = newData) %>% clearMarkers() %>%
                addMarkers( ~ decimalLongitude, ~ decimalLatitude)
        }
    })
    
    
    output$fifthtext <- renderPrint({
        a <- event_data("plotly_click", source = "reactiveBars")
        return(a$y)
    })
    
    #Time.......................
    
    output$fifthmonth <- renderPlotly({
        a <- fifthtime()
        a <- arrange(a, as.numeric(a$monthofYear))
        a <- a[c("genus", "monthofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = "genus",
                variable = monthofYear,
                value = Freq
            )
        plot_ly(a, source = "reactMonth", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
            layout(title = "Features",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))%>% onRender(js)   
        })
    
    output$fifthday <- renderPlotly({
        a <- fifthtime()
        a <- arrange(a, as.numeric(a$dayofYear))
        a <- a[c("genus", "dayofYear")]
        a <-
            data.frame(table(a)) %>% rename(
                group = genus,
                variable = dayofYear,
                value = Freq
            )
        plot_ly(a,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
            layout(title = "Features",
                   xaxis = list(title = "Day"),
                   yaxis = list(title = "Value"))     
        
    })
    
    observe({
        brush <- event_data("plotly_click", source = "reactMonth")
        a <- fifthtime()
        
        
        if (is.null(brush)){
            a <- arrange(a, as.numeric(a$dayofYear))
            if(!is.null(input$trace)){
                a <- a %>% filter(genus %in% input$trace)
            }
            a <- a[c("genus", "dayofYear")]
            a <-
                data.frame(table(a)) %>% rename(
                    group = genus,
                    variable = dayofYear,
                    value = Freq
                )
            output$fifthday <- renderPlotly({
                plot_ly(a,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                    layout(title = "Features",
                           xaxis = list(title = "Day"),
                           yaxis = list(title = "Value")) %>% onRender(js)
            })
        } else {
            select = as.data.frame(brush$x)
            newData <- a %>% filter(monthofYear %in% select$'brush$x')
            if(!is.null(input$trace)){
                newData <- newData %>% filter(genus %in% input$trace)
            }
            newData<-arrange(newData,as.numeric(newData$dayofYear))
            newData<- newData[c("genus", "dayofYear")]
            newData <- data.frame(table(newData)) %>%rename(group = genus,
                                                            variable = dayofYear,
                                                            value = Freq)
            output$fifthday <- renderPlotly({
                plot_ly(newData,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                    layout(title = "Features",
                           xaxis = list(title = "Day"),
                           yaxis = list(title = "Value"))%>% onRender(js) 
            })
        }
    })
    
    observeEvent(input$reset,{
        output$fifthday <- renderPlotly({
            a <- fifthtime()
            a <- arrange(a, as.numeric(a$dayofYear))
            a <- a[c("genus", "dayofYear")]
            a <-
                data.frame(table(a)) %>% rename(
                    group = genus,
                    variable = dayofYear,
                    value = Freq
                )
            plot_ly(a,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                layout(title = "Features",
                       xaxis = list(title = "Day"),
                       yaxis = list(title = "Value"))     
            
        })
        output$fifthmonth <- renderPlotly({
            a <- fifthtime()
            a <- arrange(a, as.numeric(a$monthofYear))
            a <- a[c("genus", "monthofYear")]
            a <-
                data.frame(table(a)) %>% rename(
                    group = "genus",
                    variable = monthofYear,
                    value = Freq
                )
            plot_ly(a, source = "reactMonth", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                layout(title = "Features",
                       xaxis = list(title = ""),
                       yaxis = list(title = ""))%>% onRender(js)   
        })
        
    })
    
    #for plotly selected
    observe({
        brush <- event_data("plotly_selected", source = "reactMonth")
        a <- fifthtime()
        
        
        if (is.null(brush)){
            a <- arrange(a, as.numeric(a$dayofYear))
            a <- a[c("genus", "dayofYear")]
            a <-
                data.frame(table(a)) %>% rename(
                    group = genus,
                    variable = dayofYear,
                    value = Freq
                )
            output$fifthday <- renderPlotly({
                plot_ly(a,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                    layout(title = "Features",
                           xaxis = list(title = "Day"),
                           yaxis = list(title = "Value")) %>% onRender(js)
            })
        } else {
            select = as.data.frame(brush$x)
            newData <- a %>% filter(monthofYear %in% select$'brush$x')
            newData<-arrange(newData,as.numeric(newData$dayofYear))
            newData<- newData[c("genus", "dayofYear")]
            newData <- data.frame(table(newData)) %>%rename(group = genus,
                                                            variable = dayofYear,
                                                            value = Freq)
            output$fifthday <- renderPlotly({
                plot_ly(newData,source = "reactDay", x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
                    layout(title = "Features",
                           xaxis = list(title = "Day"),
                           yaxis = list(title = "Value")) %>% onRender(js)
            })
        }
    })
    
    
    
    
    
    output$fifthtext2 <- renderPrint({
        # brush <- event_data("plotly_click", source = "reactDay")
        # select = as.data.frame(brush$x)
        # newData <- a %>% filter(monthofYear %in% select$'brush$x')
        # newData<-arrange(newData,as.numeric(newData$dayofYear))
        # newData<- newData[c("genus", "dayofYear")]
        # newData <- data.frame(table(newData)) %>%rename(group = genus,
        #                                                 variable = dayofYear,
        #                                                 value = Freq)
        # return(newData)
        d <- input$trace
        if (is.null(d)) "Clicked item appear here" else d    })
    
    
    #Some more examples of reactive plots......................................
    
    # for maintaining the current category (i.e. selection)
    current_category <- reactiveVal()
    
    # report sales by category, unless a category is chosen
    sales_data <- reactive({
        if (!length(current_category())) {
            return(count(mammals, genus))
        }
        mammals %>%
            filter(genus %in% current_category()) %>%
            count(species)
    })
    
    # Note that pie charts don't currently attach the label/value 
    # with the click data, but we can leverage `customdata` as a workaround
    output$fifthpiechart <- renderPlotly({
        d <- setNames(sales_data(), c("labels", "values"))
        plot_ly(d, source = "reactpie") %>%
            add_pie(labels = ~labels, values = ~values, customdata = ~labels) %>%
            layout(title = current_category() %||% "Total Sales")
    })
    
    # update the current category if the clicked value matches a category
    observe({
        cd <- event_data("plotly_click", source = "reactpie")$customdata[[1]]
        if (isTRUE(cd %in% categories)) current_category(cd)
    })
    
    # populate back button if category is chosen
    output$back <- renderUI({
        if (length(current_category())) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    # clear the chosen category on back button press
    observeEvent(input$clear, current_category(NULL))
    #Pie chart End here.......................................
    
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
            add_bars(x = ~Names, y = ~value, color = ~Names) %>%
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
            add_lines(x = ~month, y = ~value, color = ~color)
    })
    
    # update the current category if the clicked value matches a category
    observe({
        cd <- event_data("plotly_click", source = "barwithtime")$x
        if (isTRUE(cd %in% categoriesbar)) current_categorybar(cd)
    })
    
    # populate back button if category is chosen
    output$back1 <- renderUI({
        if (length(current_categorybar())) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    # clear the chosen category on back button press
    observeEvent(input$clear, current_categorybar(NULL))

    #Bar chart End here.....................................
    
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
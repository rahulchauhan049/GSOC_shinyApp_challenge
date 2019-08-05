# Module UI
  
#' @title   mod_dataSummary_ui and mod_dataSummary_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataSummary
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import flexdashboard
#' @import bdvis

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(treemap)
library(sunburstR)
mod_dataSummary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(style='padding-bottom:0px;',
      column(3, flexdashboard::gaugeOutput(ns("Gauge1"))),
      column(3, flexdashboard::gaugeOutput(ns("Gauge2"))),
      column(3, flexdashboard::gaugeOutput(ns("Gauge3"))),
      column(3, flexdashboard::gaugeOutput(ns("Gauge4")))
             
      ),
  # fluidRow(column(12,plotOutput(ns("gauge"), height = "150px"))),br(),
  fluidRow(style='padding-top:-50px;',
    column(3,style='padding-top:-50px;',
             div(
               class = "center",
               fluidRow(
                valueBoxOutput((ns("boxA")), width = "100%")),
               fluidRow(  
                 valueBoxOutput((ns("boxB")), width = "100%")),
               fluidRow(
                   valueBoxOutput((ns("boxC")), width = "100%"))
               )
    )
    ,column(9,style='padding:20px;',tabsetPanel(
      tabPanel("Spatial",fluidRow(column(3,style='padding:20px;',fluidRow(
                                valueBoxOutput(ns("totalCountry"), width = "40%")),
                                fluidRow(
                                valueBoxOutput(ns("naCountry"), width = "40%"))),
               column(9,plotlyOutput(ns("countryBar")))
               )),
      tabPanel("Temporal", selectInput(
        ns("barselect"),
        "Select Column to be displayed",
        c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species"),
        selected = "basisOfRecord"
      ),plotlyOutput(ns("bar"), height = "50%")),
      tabPanel("Taxonomic",
               sunburstOutput(ns("sunbrust"), height = "350px"))
    ))
  )#End of fluidRow
  )
}
    
# Module Server
    
#' @rdname mod_dataSummary
#' @export
#' @keywords internal
    
mod_dataSummary_server <- function(input, output, session, dataset){
  
  ns <- session$ns
  
  output$Gauge1 <- flexdashboard::renderGauge({
    df <- dataset()
      latitude <- round(((nrow(df["decimalLatitude"])-sum(is.na(df["decimalLatitude"])))/nrow(df["decimalLatitude"])), 2)*100
      longitude <- round(((nrow(df["decimalLongitude"])-sum(is.na(df["decimalLongitude"])))/nrow(df["decimalLongitude"])), 2)*100
      if(latitude>longitude){
        geo <- longitude
      } else {
        geo <- latitude
      }
    gauge(geo, min = 0, max = 100, symbol = "%", label = "% of Plotable\nGeo coordinates", gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    )) 
  })   
  
  output$Gauge2 <- flexdashboard::renderGauge({
    df <- dataset()
    dateIdentified <- round(((nrow(df["dateIdentified"])-sum(is.na(df["dateIdentified"])))/nrow(df["dateIdentified"])), 2)*100
    gauge(dateIdentified, min = 0, max = 100, symbol = "%", label = "% of rows\nwith dateIdentified records", gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    )) 
  })  
  
  output$Gauge3 <- flexdashboard::renderGauge({
    df <- dataset()
    occurrenceRemarks <- round(((nrow(df["occurrenceRemarks"])-sum(is.na(df["occurrenceRemarks"])))/nrow(df["occurrenceRemarks"])), 2)*100
    gauge(occurrenceRemarks, min = 0, max = 100, symbol = "%", label = "% of rows\nwith occurence remark", gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    )) 
  })

output$Gauge4 <- flexdashboard::renderGauge({
  df <- dataset()
  eventTime <- round(((nrow(df["eventTime"])-sum(is.na(df["eventTime"])))/nrow(df["eventTime"])), 2)*100
  gauge(eventTime, min = 0, max = 100, symbol = "%", label = "% of rows\nwith eventTime records", gaugeSectors(
    success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  )) 
})  
  

  
  output$boxA <- renderValueBox({valueBox(
    value = (nrow(dataset()["decimalLatitude"])),
    subtitle = "# of Records\n(# of Geo-coordinates)",
    icon = icon("compass"),
    color = "aqua",
    width = 1
  )})
  output$boxB <- renderValueBox({valueBox(
    value = nrow(unique(dataset()["scientificName"])),
    subtitle = "# of Taxa",
    icon = icon("file-signature"),
    color = "aqua",
    width = 1
  )})
  output$boxC <- renderValueBox({valueBox(
    value = length(dataset()),
    subtitle = "# of Attributes",
    icon = icon("area-chart"),
    color = "aqua",
    width = 1
  )})
  
  formattedData <- reactive({
    dataset <- dataset()
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
      dataForBar <-
        cbind(dataForBar[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")], dayofYear, weekofYear, monthofYear, Year_)
      
    } else {
      stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
    }
    return(dataForBar)
  })
  

  output$bar <- renderPlotly({
    dataForBar <- arrange(formattedData(), as.numeric(formattedData()$Year_))
    dataForBar <- dataForBar[c(input$barselect, "Year_")]
    
    dataForBar <-
      data.frame(table(dataForBar)) %>% dplyr::rename(group = input$barselect,
                                               variable = Year_,
                                               value = Freq)
    plot_ly(
      dataForBar,
      source = "barselected",
      x = ~ value,
      y = ~ variable,
      color = ~ group
    )%>%  layout(showlegend = FALSE, height = 250) %>%
      add_bars()

    
  })  
  output$totalCountry <- renderValueBox({valueBox(
    value = nrow(unique(dataset()["countryCode"])),
    subtitle = "# of Countries",
    icon = icon("area-chart"),
    color = "aqua",
    width = 1
  )})
  
  output$naCountry <- renderValueBox({valueBox(
    value = rowSums(is.na(dataset()["countryCode"])),
    subtitle = "# Missing country",
    icon = icon("area-chart"),
    color = "aqua",
    width = 1
  )})
  
  output$countryBar <- renderPlotly({
    country <- data.frame(table(na.omit(dataset()["countryCode"])))%>%dplyr::rename(
      CountryName = Var1,
      NumberOfRecords = Freq)
    plot_ly(data = country,
            x = ~CountryName,
            y = ~NumberOfRecords,
            name = "Countries",
            type = "bar"
    ) %>% layout(showlegend = FALSE, height = 350)
    
  })
  
  output$sunbrust <- renderSunburst({
    data <- dataset()
    if (!nrow(data[-which(data[, "genus"] == ""),]) == 0) {
      data <- data[-which(data[, "genus"] == ""),]
    }
    if (!nrow(data[-which(data[, "family"] == ""),]) == 0) {
      data <- data[-which(data[, "family"] == ""),]
    }
    if (!nrow(data[-which(data[, "order"] == ""),]) == 0) {
      data <- data[-which(data[, "order"] == ""),]
    }
    if (!nrow(data[-which(data[, "phylum"] == ""),]) == 0) {
      data <- data[-which(data[, "phylum"] == ""),]
    }
    data <- arrange(data, family)
    temp <- as.data.frame(table(data["genus"]))
    data <- unique(data)
    temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
    temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
    temp <- temp %>%
      mutate(path = paste(phylum, order, family, genus, sep="-")) %>%
      dplyr::select(path, Freq)
    
    # Plot
    sunburst(temp, legend=FALSE)
  })
  
  
  }
  ## To be copied in the UI
# mod_dataSummary_ui("dataSummary_ui_1")
    
## To be copied in the server
# callModule(mod_dataSummary_server, "dataSummary_ui_1")
 

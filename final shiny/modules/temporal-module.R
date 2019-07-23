temporalUI <- function(id, label = "Temporal Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    box(
      title = "Year Bar Chart",
      status = "primary",
      solidHeader = TRUE,
      selectInput(
        ns("barselect"),
        "Select Column to be displayed",
        c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species"),
        selected = "basisOfRecord"
      ),
      plotlyOutput(ns("bar"))
    ),
    box(
      title = "Pie Chart",
      status = "primary",
      solidHeader = TRUE,
      selectInput(
        ns("pieselect"),
        "Select Column to be displayed",
        c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species"),
        selected = "basisOfRecord"
      ),
      plotlyOutput(ns("pie"))
    ),
    box(title = "Pie Chart",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("roseplot"))
        )
  ))
}

temporalServer <- function(input, output, session, dataset) {
  dataForBarReact <- reactive({
    dataset <- read.csv("smallData.csv")
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
  
  datasetForPlotly <- reactive({

  dataForBar <- arrange(dataForBar, as.numeric(dataForBar$Year_))
  dataForBar <- dataForBar[c(input$barselect, "Year_")]
  
  dataForBar <-
    data.frame(table(dataForBar)) %>% rename(group = input$barselect,
                                             variable = Year_,
                                             value = Freq)
  return(dataForBar)
  })
  
  output$bar <- renderPlotly({
    plot_ly(
      datasetForPlotly(),
      source = "barselected",
      x = ~ variable,
      y = ~ value,
      color = ~ group
    ) %>%
      add_bars()
    
  })
  
  output$pie <- renderPlotly({
    if (input$pieselect == "kingdom") {
      label <- ~ kingdom
    } else if (input$pieselect == "phylum") {
      label <- ~ phylum
    } else if (input$pieselect == "family") {
      label <- ~ family
    } else if (input$pieselect == "genus") {
      label <- ~ genus
    } else if (input$pieselect == "species") {
      label <- ~ species
    } else if (input$pieselect == "order"){
      label <- ~ order
    } else {
      label <- ~ basisOfRecord
    }
    if (!nrow(sample[-which(sample[, input$pieselect] == ""),]) == 0) {
      dataa <- sample[-which(sample[, input$pieselect] == ""),]
    } else {
      dataa <- sample
    }
    
    plot_ly(
      data = na.omit(dataa[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")]),
      labels = label,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text'
    )
  })
  
  observe({
  select <- event_data("plotly_click", source = "barselected")
  if (is.null(select)){
    output$pie <- renderPlotly({
      if (input$pieselect == "kingdom") {
        label <- ~ kingdom
      } else if (input$pieselect == "phylum") {
        label <- ~ phylum
      } else if (input$pieselect == "family") {
        label <- ~ family
      } else if (input$pieselect == "genus") {
        label <- ~ genus
      } else if (input$pieselect == "species") {
        label <- ~ species
      } else if (input$pieselect == "order"){
        label <- ~ order
      } else {
        label <- ~ basisOfRecord
      }
      if (!nrow(dataset[-which(dataset[, input$pieselect] == ""),]) == 0) {
        dataa <- dataset[-which(dataset[, input$pieselect] == ""),]
      } else {
        dataa <- dataset
      }
      
      plot_ly(
        data = na.omit(dataa[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")]),
        labels = label,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text'
      )
    })
  } else {
    newData <- dataset %>% filter(year %in% as.numeric(select))
    output$pie <- renderPlotly({
      if (input$pieselect == "kingdom") {
        label <- ~ kingdom
      } else if (input$pieselect == "phylum") {
        label <- ~ phylum
      } else if (input$pieselect == "family") {
        label <- ~ family
      } else if (input$pieselect == "genus") {
        label <- ~ genus
      } else if (input$pieselect == "species") {
        label <- ~ species
      } else if (input$pieselect == "order"){
        label <- ~ order
      } else {
        label <- ~ basisOfRecord
      }
      if (!nrow(newData[-which(newData[, input$pieselect] == ""),]) == 0) {
        newData <- newData[-which(newData[, input$pieselect] == ""),]
      } 
      
      plot_ly(
        data = na.omit(newData[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")]),
        labels = label,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text'
      )
    })
  }
  
    })
  
  
  output$roseplot <- renderPlot({
    dataForRose <-
      cbind(dataset[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")], dayofYear, weekofYear, monthofYear, Year_)
    dataForRose <- arrange(dataForRose, as.numeric(dataForRose$monthofYear))
    dataForRose <- dataForRose[c("basisOfRecord", "monthofYear")]
    if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]) == 0) {
      dataForRose <- dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]
    } 
    dataForRose <-
      data.frame(table(dataForRose)) %>% rename(
        group = basisOfRecord,
        variable = monthofYear,
        value = Freq
      )
    ggplot(data = dataForRose, aes(
      x = variable,
      y = value,
      fill = group
    )) +
      geom_bar(stat = "identity") +
      coord_polar() + xlab("") + ylab("")    
    
  })
  
  observe({
    select <- event_data("plotly_click", source = "barselected")
    if(is.null(select)){
      output$roseplot <- renderPlot({
        dataForRose <-
          cbind(dataset[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")], dayofYear, weekofYear, monthofYear, Year_)
        dataForRose <- arrange(dataForRose, as.numeric(dataForRose$monthofYear))
        dataForRose <- dataForRose[c("basisOfRecord", "monthofYear")]
        if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]) == 0) {
          dataForRose <- dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]
        } 
        dataForRose <-
          data.frame(table(dataForRose)) %>% rename(
            group = basisOfRecord,
            variable = monthofYear,
            value = Freq
          )
        ggplot(data = dataForRose, aes(
          x = variable,
          y = value,
          fill = group
        )) +
          geom_bar(stat = "identity") +
          coord_polar() + xlab("") + ylab("")    
      })
    } else {
      output$roseplot <- renderPlot({
      dataForRose <-
        cbind(dataset[c("basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species")], dayofYear, weekofYear, monthofYear, Year_)
      dataForRose <- dataForRose %>% filter(Year_ %in% as.numeric(select))
      dataForRose <- arrange(dataForRose, as.numeric(dataForRose$monthofYear))
      dataForRose <- dataForRose[c("basisOfRecord", "monthofYear")]
      if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]) == 0) {
        dataForRose <- dataForRose[-which(dataForRose[, "basisOfRecord"] == ""),]
      } 
      dataForRose <-
        data.frame(table(dataForRose)) %>% rename(
          group = basisOfRecord,
          variable = monthofYear,
          value = Freq
        )
      ggplot(data = dataForRose, aes(
        x = variable,
        y = value,
        fill = group
      )) +
        geom_bar(stat = "identity") +
        coord_polar() + xlab("") + ylab("") 
      })
    }
  })
  
}

library(shiny)
library(dplyr)
library("rgbif")

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
mammals <- cbind(mammals[c("genus", "species")],dayofYear,weekofYear,monthofYear,Year_)
mammals<-arrange(mammals,as.numeric(mammals$monthofYear))
mammals<- mammals[c("genus", "species", "monthofYear")]
mammals <- na.omit(mammals)
categories <- unique(mammals$genus)


ui <- fluidPage(
  plotlyOutput("bar"),
  uiOutput("back"),
  plotlyOutput("time")
)

server <- function(input, output, session) {
  
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
  
  # the pie chart
  output$bar <- renderPlotly({
    d <- setNames(sales_data(), c("x", "y"))
    
    plot_ly(d) %>%
      add_bars(x = ~x, y = ~y, color = ~x) %>%
      layout(title = current_category() %||% "Total Sales")
  })
  
  # same as sales_data
  sales_data_time <- reactive({
    if (!length(current_category())) {
      return(count(mammals, genus, monthofYear))
    }
    mammals %>%
      filter(genus %in% current_category()) %>%
      count(species, monthofYear)
  })
  
  output$time <- renderPlotly({
    d <- setNames(sales_data_time(), c("color", "x", "y"))
    plot_ly(d) %>%
      add_lines(x = ~x, y = ~y, color = ~color)
  })
  
  # update the current category if the clicked value matches a category
  observe({
    cd <- event_data("plotly_click")$x
    if (isTRUE(cd %in% categories)) current_category(cd)
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(current_category())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, current_category(NULL))
}

shinyApp(ui, server)
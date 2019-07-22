taxonomicTabUi <- function(id, label = "Taxonomic Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    box(
      title = "Histogram",
      status = "primary",
      solidHeader = TRUE,
      selectizeInput(ns("taxoBarInput"), "Select Taxonomic Level", 
                     c("Kingdom", "Phylum", "Order", "Family", "Genus", "Species"),
                     selected = "Order"),
      plotlyOutput(ns("taxonomicBar"))
    ),
    box(
      title = "Histogram",
      status = "primary",
      solidHeader = TRUE,
      circlepackeROutput(ns("circleplot"))
      )
  )
  )
}

taxonomicTabServer <- function(input, output, session, dataset) {

  output$taxonomicBar <- renderPlotly({
    if (input$taxoBarInput == "Kingdom") {
      label <- ~ kingdom
    } else if (input$taxoBarInput == "Phylum") {
      label <- ~ phylum
    } else if (input$taxoBarInput == "Family") {
      label <- ~ family
    } else if (input$taxoBarInput == "Genus") {
      label <- ~ genus
    } else if (input$taxoBarInput == "Species") {
      label <- ~ species
    } else{
      label <- ~ order
    }
    plot_ly(data = dataset, y =label, source = "reactiveBars")
  })

  output$circleplot <- renderCirclepackeR({
    data <- formatData(dataset)
    data$pathString <- paste("Vis", data$group, data$subgroup, data$subsubgroup, sep = "/")
    population <- as.Node(data)
    # Make the plot
    circlepackeR(population, size = "value")
  })
  
  observe({
    select <- event_data("plotly_click", source ="reactiveBars")
    if(is.null(select)){
      output$circleplot <- renderCirclepackeR({
        data <- formatData(dataset)
        data$pathString <- paste("Vis", data$group, data$subgroup, data$subsubgroup, sep = "/")
        population <- as.Node(data)
        # Make the plot
        circlepackeR(population, size = "value")
      })
    } else {
      if (input$taxoBarInput == "Kingdom") {
        newData <- dataset %>% filter(kingdom %in% select)
      } else if (input$taxoBarInput == "Phylum") {
        newData <- dataset %>% filter(phylum %in% select)
      } else if (input$taxoBarInput == "Family") {
        newData <- dataset %>% filter(family %in% select)
      } else if (input$taxoBarInput == "Genus") {
        newData <- dataset %>% filter(genus %in% select)
      } else if (input$taxoBarInput == "Species") {
        newData <- dataset %>% filter(species %in% select)
      } else{
        newData <- dataset %>% filter(order %in% select)
      }
      if(nrow(newData)==0){
        output$circleplot <- renderCirclepackeR({
          data <- formatData(dataset)
          data$pathString <- paste("Vis", data$group, data$subgroup, data$subsubgroup, sep = "/")
          population <- as.Node(data)
          # Make the plot
          circlepackeR(population, size = "value")
        })
      } else {
      output$circleplot <- renderCirclepackeR({
        data <- formatData(newData)
        data$pathString <- paste("Vis", data$group, data$subgroup, data$subsubgroup, sep = "/")
        population <- as.Node(data)
        # Make the plot
        circlepackeR(population, size = "value")
      })
      }
    }
    })
  
}

#Functions.................................................
formatData <- function(data){
  data <- na.omit(data[c("phylum", "order", "family", "genus")])
  if (!nrow(data[-which(data[, "phylum"] == ""),]) == 0) {
    data <- data[-which(data[, "phylum"] == ""),]
  }
  if (!nrow(data[-which(data[, "order"] == ""),]) == 0) {
    data <- data[-which(data[, "order"] == ""),]
  } 
  if (!nrow(data[-which(data[, "family"] == ""),]) == 0) {
    data <- data[-which(data[, "family"] == ""),]
  } 
  if (!nrow(data[-which(data[, "genus"] == ""),]) == 0) {
    data <- data[-which(data[, "genus"] == ""),]
  } 
  data <- arrange(data, family)
  temp <- as.data.frame(table(data["genus"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
  temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
  colnames(temp) <- c("root","group","subgroup", "subsubgroup","value")
  
  return(temp)
}

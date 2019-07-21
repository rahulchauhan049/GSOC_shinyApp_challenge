
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
    )
  ))
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
}
spatialUI <- function(id, label = "Spatial Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    verbatimTextOutput(ns("a"))
  ))
}

spatialServer <- function(input, output, session, dataset) {
  output$a <- renderPrint("Adasdsad")
}
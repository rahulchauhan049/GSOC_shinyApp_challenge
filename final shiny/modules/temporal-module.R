temporalUI <- function(id, label = "Temporal Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    verbatimTextOutput(ns("a"))
  ))
}

temporalServer <- function(input, output, session, dataset) {
  output$a <- renderPrint("Adasdsad")
}
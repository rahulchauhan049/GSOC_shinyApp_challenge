taxonomicTabUi <- function(id, label = "Taxonomic Plots"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    column(width = 12, DT::dataTableOutput(ns("tableoutput")))
  ))
}

taxonomicTabServer <- function(input, output, session){
  output$inputDataTable <- DT::renderDataTable(DT::datatable({
    summarizeDataframe(returnData)
  }, options = list(scrollX = TRUE)))
}
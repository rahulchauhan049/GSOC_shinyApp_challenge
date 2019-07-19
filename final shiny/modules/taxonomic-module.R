taxonomicTabUi <- function(id, label = "Taxonomic Plots"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    column(width = 12, "adasd", DT::dataTableOutput(ns("tableoutput")))
  ))
}

taxonomicTabServer <- function(input, output, session, dataset){
  output$tableoutput <- DT::renderDataTable(DT::datatable(dataset))
}
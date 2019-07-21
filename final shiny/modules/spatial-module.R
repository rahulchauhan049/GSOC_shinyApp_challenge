spatialUI <- function(id, label = "Spatial Plots") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(fluidRow(
    box(
      title = "Bar Plot",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput(ns("bar")),
      verbatimTextOutput(ns("a"))
    )
  ))
}

spatialServer <- function(input, output, session, dataset) {
  output$bar <- renderPlotly({
    data <- as.data.frame(table(na.omit(dataset["identifiedBy"])))
    plot_ly(
      data = data,
      source = "barsource",
      x = ~ Var1,
      y = ~ Freq
    )
  })
  output$a <- renderPrint(event_data("plotly_click", source = "barsource"))
}

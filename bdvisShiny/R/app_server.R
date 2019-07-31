#' @import shiny
options(shiny.maxRequestSize = 5000 * 1024 ^ 2)

inputDataset <- data.frame()

app_server <- function(input, output,session) {
  inputDataset <- callModule(mod_input_server, "input_ui_1")
  # List the first level callModules here
}

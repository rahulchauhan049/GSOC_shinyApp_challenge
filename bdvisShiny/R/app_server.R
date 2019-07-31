#' @import shiny
options(shiny.maxRequestSize = 5000 * 1024 ^ 2)

inputDataset <- data.frame()

app_server <- function(input, output,session) {
  
  ##################Input tabs#################################

    inputDataset <- callModule(mod_input_server, "input_ui_1")
  
  ################End of Input Tab#############################
    
    
    
  ##################Spatial tabs###############################
    
    callModule(mod_spatial_server, "spatial_ui_1", inputDataset())
    
  ################End of Input Tab#############################

    
}

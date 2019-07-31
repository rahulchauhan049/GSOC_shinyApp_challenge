# Module UI
  
#' @title   mod_spatial_ui and mod_spatial_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_spatial
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_spatial_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_spatial
#' @export
#' @keywords internal
    
mod_spatial_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_spatial_ui("spatial_ui_1")
    
## To be copied in the server
# callModule(mod_spatial_server, "spatial_ui_1")
 

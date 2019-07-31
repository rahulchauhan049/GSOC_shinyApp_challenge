# Module UI
  
#' @title   mod_temporal_ui and mod_temporal_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_temporal
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_temporal_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_temporal
#' @export
#' @keywords internal
    
mod_temporal_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_temporal_ui("temporal_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_server, "temporal_ui_1")
 

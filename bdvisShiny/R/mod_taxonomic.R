# Module UI
  
#' @title   mod_taxonomic_ui and mod_taxonomic_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_taxonomic
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_taxonomic_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_taxonomic
#' @export
#' @keywords internal
    
mod_taxonomic_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_taxonomic_ui("taxonomic_ui_1")
    
## To be copied in the server
# callModule(mod_taxonomic_server, "taxonomic_ui_1")
 

# Module UI
  
#' @title   mod_dataSummary_ui and mod_dataSummary_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataSummary
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

library(ggplot2)
mod_dataSummary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
  fluidRow(column(12,plotOutput(ns("gauge"), height = "200px"))),
  fluidRow(
    tagList(
      column(12,
             div(
               class = "center",
               fluidRow(
                 infoBox("# of Records", textOutput(ns("inputDataRows")), icon = icon("list-ol")),
                 infoBox(
                   "# of Fields",
                   textOutput(ns("inputDataColumns")),
                   icon = icon("th-list"),
                   color = "purple"
                 ),
                 infoBox(
                   "# of Scientific Names",
                   textOutput(ns("inputDataSpecies")),
                   icon = icon("paw"),
                   color = "yellow"
                 )
               )))
      
    )
  )#End of fluidRow
  )
}
    
# Module Server
    
#' @rdname mod_dataSummary
#' @export
#' @keywords internal
    
mod_dataSummary_server <- function(input, output, session, dataset){
  
  ns <- session$ns
  output$inputDataRows <- renderText(nrow(dataset()))
  output$inputDataColumns <- renderText(length(dataset()))
  output$inputDataSpecies <-
    renderText(nrow(unique(dataset()["scientificName"])))
  output$gauge <- renderPlot({
    gauge(dataset())
  },bg = "grey")
  
  #Function#########################
  gauge <- function(df){
    latitude <- round(((nrow(df["decimalLatitude"])-sum(is.na(df["decimalLatitude"])))/nrow(df["decimalLatitude"])), 2)
    longitude <- round(((nrow(df["decimalLongitude"])-sum(is.na(df["decimalLongitude"])))/nrow(df["decimalLongitude"])), 2)
    if(latitude>longitude){
      geo <- longitude
    } else {
      geo <- latitude
    }
    dateIdentified <- round(((nrow(df["dateIdentified"])-sum(is.na(df["dateIdentified"])))/nrow(df["dateIdentified"])), 2)
    occurrenceRemarks <- round(((nrow(df["occurrenceRemarks"])-sum(is.na(df["occurrenceRemarks"])))/nrow(df["occurrenceRemarks"])), 2)
    eventTime <- round(((nrow(df["eventTime"])-sum(is.na(df["eventTime"])))/nrow(df["eventTime"])), 2)
    
    
    
    df <- data.frame(variable = c("geo", "dateIdentified", "occurrenceRemarks", "eventTime"), percentage = c(geo, dateIdentified, occurrenceRemarks, eventTime))
    df <- df %>% mutate(group=ifelse(percentage <0.6, "red",
                                     ifelse(percentage>=0.6 & percentage<0.8, "orange","green")),
                        label=paste0(percentage*100, "%"),
                        title=dplyr::recode(variable, `geo`="% of Plotable\nGeo coordinates",
                                            `dateIdentified`="% of rows\nwith dateIdentified records",
                                            `occurrenceRemarks`="% of rows\n with occurence Remarks",
                                            `eventTime`="% of rows\nwith eventTime records"
                        ))
    ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
      geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
      geom_rect() + 
      coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
      geom_text(aes(x = 0, y = 0, label = label, colour=group), size=6.5, family="Poppins SemiBold") +
      geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=4.2) + 
      facet_wrap(~title, ncol = 5) +
      theme_void() +
      scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank(),panel.background = element_blank(),
            plot.background = element_rect(fill = "grey")) +
      guides(fill=FALSE) +
      guides(colour=FALSE)
    
  }
  }
    
## To be copied in the UI
# mod_dataSummary_ui("dataSummary_ui_1")
    
## To be copied in the server
# callModule(mod_dataSummary_server, "dataSummary_ui_1")
 

#' @import shiny
#' @import data.table
#' @import shinydashboard
#' @import finch
#' @import DT
#' @import bdchecks
#' @import leaflet
#' @import shinyjs
#' @import plotly
#' @import dplyr
#' @import bdvis
#' @import nycflights13
#' @import ggstat
#' @import purrr
#' @import data.table
#' @import circlepackeR
#' @import collapsibleTree
#' @import networkD3
#' @import bdchecks
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bdchecks))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(bdvis))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(ggstat))
suppressPackageStartupMessages(library(purrr)) # just for `%||%`
suppressPackageStartupMessages(library(data.tree))
suppressPackageStartupMessages(library(circlepackeR))
suppressPackageStartupMessages(library(collapsibleTree))
suppressPackageStartupMessages(library(networkD3))
suppressPackageStartupMessages(library(bdchecks))

app_ui <- function() {
  
  dashboardPage(skin="green",
    dashboardHeader(title="bdvis dashboard"),
    
    #----------------------SideBar Start-------------------------------------
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data Input", tabName = "dataInputTab", icon = icon("database")),
        menuItem("Data Summary", tabName = "dataSummary", icon = icon("database")),
        menuItem("Spatial Visualization", tabName = "spatialTab", icon = icon("map-marked")),
        menuItem("Taxonomic Visualization", tabName = "taxonomicTab", icon = icon("connectdevelop")),
        menuItem("Temporal Visualization", tabName = "temporalTab", icon = icon("clock"))
      )#Sidebar menu ends here
    ),#sidebar Dashboard ends here
    #----------------------SideBar End-------------------------------------
    
    
    
    #----------------------Body Start-------------------------------------
    dashboardBody(
      golem_add_external_resources(),
      # Boxes need to be put in a row (or column)
      tabItems(
         
        tabItem(tabName = "dataSummary",
                # -------------------------------
                mod_input_ui("input_ui_1")
                # -------------------------------
        ),
        
        
        tabItem(tabName = "dataInputTab",
                # -------------------------------
                mod_dataInput_ui("dataInput_ui_1")
                # -------------------------------
        )
        
        
        # tabItem(tabName = "spatialTab",
        #         # -------------------------------
        # 
        #         mod_spatial_ui("spatial_ui_1")
        # 
        #         # -------------------------------
        # ),
        # 
        # 
        # 
        # tabItem(tabName = "taxonomicTab",
        #         # -------------------------------
        # 
        #         mod_taxonomic_ui("taxonomic_ui_1")
        # 
        #         # -------------------------------
        # ),
        # 
        # 
        # tabItem(tabName = "temporalTab",
        #         # -------------------------------
        # 
        #         mod_temporal_ui("temporal_ui_1")
        # 
        #         # -------------------------------
        # )

      )  
    )#Dashboard Body ends here
    
    #-------------------------Body Ends Here-------------------------------
    
  )#End of dashboard page
}

#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'bdvisShiny')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bdchecks))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(shinyjs))





shinyUI(dashboardPage(
  dashboardHeader(title = "bdvis dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "dataInputTab", icon = icon("database")),
      menuItem("Spatial Visualization", tabName = "spatialTab", icon = icon("map-marked")),
      menuItem("Taxonomic Visualization", tabName = "taxonomicTab", icon = icon("connectdevelop")),
      menuItem("Temporal Visualization", tabName = "temporalTab", icon = icon("clock"))
    )#Sidebar menu ends here
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "dataInputTab",fluidRow(div(
        # -------------------------------
        
        bdFileInput("bdFileInput", "User data (.csv format)")
        
        # -------------------------------
        
        
      ))),
      tabItem(tabName = "spatialTab"),
      tabItem(tabName = "taxonomicTab"),
      tabItem(tabName = "temporalTab")
    )  
  )#Dashboard Body ends here
))#ShinyUi and dashboard Page ends here

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(bdchecks))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(bdvis))
suppressPackageStartupMessages(library("nycflights13"))
suppressPackageStartupMessages(library("ggstat"))
suppressPackageStartupMessages(library("purrr")) # just for `%||%`
suppressPackageStartupMessages(library(data.tree))
suppressPackageStartupMessages(library(circlepackeR))
suppressPackageStartupMessages(library(collapsibleTree))
library("networkD3")




shinyUI(dashboardPage(
  dashboardHeader(title = "bdvis dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "dataInputTab", icon = icon("database")),
      menuItem("Spatial Visualization", tabName = "spatialTab", icon = icon("map-marked")),
      menuItem("Taxonomic Visualization", tabName = "taxonomicTab", icon = icon("connectdevelop")),
      menuItem("Temporal Visualization", tabName = "temporalTab", icon = icon("clock")),

      menuItem("temp Visualization", tabName = "temp2Tab", icon = icon("clock"))
      
    )#Sidebar menu ends here
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      
      tabItem(tabName = "dataInputTab",
      # -------------------------------
             inputTabUi("input")
     # -------------------------------
     
      ),
     
     
      tabItem(tabName = "taxonomicTab", 
        # -------------------------------
        
        taxonomicTabUi("taxo")
        
        # -------------------------------
        
        
      ),
      
      
      tabItem(tabName = "spatialTab", 
        # -------------------------------
              
        spatialUI("spatial")
              
        # -------------------------------              
      ),
      
      
      tabItem(tabName = "temporalTab",
        # -------------------------------
              
        temporalUI("temporal")
              
        # -------------------------------
      )
     
     )  
  )#Dashboard Body ends here
))#ShinyUi and dashboard Page ends here

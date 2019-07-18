library(shinydashboard)
library(shiny)

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
      tabItem(tabName = "dataInputTab"),
      tabItem(tabName = "spatialTab"),
      tabItem(tabName = "taxonomicTab"),
      tabItem(tabName = "temporalTab")
    )  
  )#Dashboard Body ends here
))#ShinyUi and dashboard Page ends here

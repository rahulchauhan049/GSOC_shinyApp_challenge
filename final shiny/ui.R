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
      tabItem(tabName = "dataInputTab", fluidRow(div(
        # -------------------------------
        
        tagList(
          column(
            12,
            h1("Add Occurrence Data"),
            column(
              3,
              
              tabsetPanel(
                type = "tabs",
                tabPanel("Existing Data",
                         selectizeInput("dataSet",
                                        "Select Sample Datasets",
                                        choices = c( "Mammals"="smallData.csv", "Hyena"="hyenaData.csv"),
                                        selected = "Mammals"
                                        ),
                         actionButton("loadexisting", "Load New Dataset")
                         ),
                # ------------- DB Module -------------------
                tabPanel(
                  "Download Data",
                  div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
                  textInput(
                    "scientificName",
                    label = h3("Scientific Name:"),
                    value = "Puma concolor"
                  ),
                  
                  numericInput(
                    "recordSize",
                    label = h3("Record Size:"),
                    value = 500
                  ),
                  
                  selectInput(
                    "hasCoords",
                    label = h3("Records Filter:"),
                    choices = list(
                      "With Coordinates" = "1",
                      "Without Coordinates" = "2",
                      "No Filter" = "3"
                    ),
                    selected = 3
                  ),
                  
                  radioButtons(
                    "queryDB",
                    label = h3("Online Database:"),
                    choices = list(
                      "GBIF (Global Biodiversity Information Facility)" = "gbif",
                      "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
                      "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
                      "Vertnet (Vertebrate Network)" = "vertnet",
                      "BISON (Biodiversity Information Serving Our Nation)" = "bison",
                      "iNaturalist" = "inat",
                      "ALA (Atlas of Living Australia)" = "ala"
                      # "OBIS (Ocean Biogeographic Information System)" = "obis",
                      # "AntWeb" = "antweb"
                    ),
                    selected = "gbif"
                  ),
                  
                  br(),
                  div(
                    id = "queryDatabaseDiv",
                    class = "activeButton",
                    actionButton("queryDatabase", "Query Database", icon("download"))
                  ),
                  br()
                ),
                
                # ------------- End of DB Module -------------------
                
                # ------------- Local Disk Module -------------------
                tabPanel(
                  "Upload Data",
                  div(class = "secondaryHeaders", h3("Option 02: From Local Disk")),
                  div(
                    id = "inputFileDiv",
                    class = "activeButton",
                    fileInput(
                      "inputFile",
                      label = h3("CSV / DWCA ZIP file input"),
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".zip",
                        "application/zip"
                      )
                    )
                  )
                ),
                # checkboxInput(ns("darwinizerControl"),
                #               label = "Perform Header Cleaning",
                #               value = TRUE),
                
                helpText(
                  "To manually edit or clean headers, use ",
                  a("bdDwC", href = "https://github.com/bd-R/bdDwC"),
                  " package."
                )
                
                
                # ------------- End of Local Disk Module -------------------
                
                
              )
              
              
            ),
            
            # ------------- Map / Table Module -------------------
            column(9,
                   tabsetPanel(
                     type = "tabs",
                     tabPanel(
                       "Map View",
                       leafletOutput("mymap", height = "700"),
                       absolutePanel(
                         top = 60,
                         right = 20,
                         selectInput(
                           "mapTexture",
                           "Map Texture",
                           choices = list(
                             "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                             "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                             "Stamen.Toner" = "Stamen.Toner",
                             "CartoDB.Positron" = "CartoDB.Positron",
                             "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
                             "Stamen.Watercolor" = "Stamen.Watercolor",
                             "Stamen.Terrain" = "Stamen.Terrain",
                             "Esri.WorldImagery" = "Esri.WorldImagery",
                             "Esri.WorldTerrain" = "Esri.WorldTerrain"
                           ),
                           selected = "CartoDB.Positron"
                         ),
                         selectInput(
                           "mapColor",
                           "Points Color",
                           choices = list(
                             "Red" = 'red',
                             "Green" = "green",
                             "Blue" = "blue",
                             "Black" = "black"
                           )
                         )
                       )
                     ),
                     tabPanel("Table View",
                              DT::dataTableOutput("inputDataTable"))
                   ))
            
            # ------------- End of Map/Table Module -------------------
          )
        )
        
        
        # -------------------------------
        
        
      ))),
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

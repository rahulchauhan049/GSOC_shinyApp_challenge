#Ui part of shiny app
shinyUI(dashboardPage(
  dashboardHeader(title = "bdvis dashboard"),
  
  #----------------------SideBar Start-------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "dataInputTab", icon = icon("database")),
      menuItem("Spatial Visualization", tabName = "spatialTab", icon = icon("map-marked")),
      menuItem("Taxonomic Visualization", tabName = "taxonomicTab", icon = icon("connectdevelop")),
      menuItem("Temporal Visualization", tabName = "temporalTab", icon = icon("clock"))
    )#Sidebar menu ends here
  ),#sidebar Dashboard ends here
  #----------------------SideBar End-------------------------------------
  
  
  
  #----------------------Body Start-------------------------------------
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
  
  #-------------------------Body Ends Here-------------------------------
  
)#End of dashboard page
)#ShinyUi and dashboard Page ends here

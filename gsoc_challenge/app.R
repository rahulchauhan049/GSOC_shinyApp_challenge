# loading packages, if not installed and installing them.
pcakages <- c( "rgbif", "bdvis", "shinythemes", 'rinat') # list of packages needed
req_packages <- pcakages[!(pcakages %in% installed.packages()[, "Package"])] # checking if the exist
if (length(req_packages) > 0) { # installing is needed
  install.packages(req_packages, dependencies = TRUE)
}
sapply(pcakages, require, character.only = TRUE)

#Import libraries
library(shiny)

#Download required tables from local system.............................................
a <- read.csv("a.csv");
country <- read.csv("countrycode.csv");

# Define UI ........................................................................................................
ui <- fluidPage(
  #GSOC Image
  div(img(src="logo.png", style="margin-top: 10px; width: 120px; float: right;", height = 50)),
    # Application title
    titlePanel("GSOC Shiny Challenge"),
    theme=shinytheme("spacelab"),# returns URL of a shiny theme
    # themeSelector(),
    navbarPage(
      title = "Google Summer of code 2019",
      id="nav",
      tabPanel("Summary", value="Summary",
      tags$div(id = "profile", style="
    box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
    padding: 8px;
    border: 2px solid #ccc;
    border-radius: 5px; /* 5px rounded corners */
    max-width: 800px;
    margin: 5% auto;
    color: black;
    background: rgba(255, 255, 255, 0.8);
  ",
                                              h3("Welcome to bdvis"),
                                              hr(),
                                              h4("Data"),
                                              p("This shiny package allows you to use functioning of bdvis package in an interactive manner. This shiny app allows you to download dataset from gbif.You can select various parameters through graphical pannels. "),
                                              hr(),
                                              h4("Visualizations"),
                                              p("This shiny also allows you to use various visualization functions on the dataset that you have downloaded"),
                                              hr(),
                                              p("To start, click in 'data' tab on the top navigation bar")
                                              
      )#End of div
               ),
      #Data Tab starts here..............................................................
      tabPanel("Data", value = "data", 
               sidebarLayout(
                 #Sidebar panel of data tab......
                 sidebarPanel(conditionalPanel(condition = "input.datatabs==1",selectizeInput(
                   'sname', 'Search Scientific Name', choices = c("Mammalia", "Animalia", "Viruses", "Archaea", "incertae sedis",
                                                                  "Protozoa", "Bacteria", "Chromista", "Fungi", "Plantae"),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
                 ),
                 selectizeInput("cntry", "Select Country", choices = country[2],
                                options = list(
                                  placeholder = 'Please select Country',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 selectizeInput(
                   'fields', 'Select Attributes to be displayed', choices = a , multiple = TRUE
                 ),
                 numericInput("limit","Enter a search limit:",value = 10,min = 10,max = 1000000),
                 actionButton("search",label="Search || Update",styleclass="primary"),
                 hr(),"Click on the download button to download dataset observation", radioButtons("dataradio", label = "Select file type", choices = c("Excel (CSV)", "Text (TSV)", "Text (Space Separated)", "Doc"), inline = TRUE),
                 downloadButton(outputId = "databutton", label = "Download Data")
                 
                 ),conditionalPanel(condition = "input.datatabs==2", textInput("queryrinat", "Query"), textInput("taxon_namerinat", "taxon_name"), numericInput("maxrinat", "No. of Obervations", value = 50),numericInput("year", "Year", value = 2019),  numericInput("month", "Month", value = 01, min = 01, max = 12), numericInput("date", "Date", value = 01, min = 01, max = 30),
                                    actionButton("searchrinat",label="Search || Update",styleclass="primary"),
                                    hr(),"Click on the download button to download dataset observation", radioButtons("dataradiorinat", label = "Select file type", choices = c("Excel (CSV)", "Text (TSV)", "Text (Space Separated)", "Doc"), inline = TRUE),
                                    downloadButton(outputId = "databuttonrinat", label = "Download Data")))
                 ,
                 
                 #Main panel of data tab..........
                 mainPanel(
                   tabsetPanel(id="datatabs", tabPanel("GBIF",value = 1, dataTableOutput('table')),
                                  tabPanel("RINAT", value = 2, dataTableOutput('rinattable')))
                   
                 ))


        ),#End of data tab
      #starting of visualization Tab
    navbarMenu("Visualization",
               tabPanel("Spatial Visualizations",
                        sidebarLayout(
                          #Sidebar of visualization tab
                          sidebarPanel(conditionalPanel(condition = "input.spatialvisualization==1",
                                                        selectizeInput(
                                                          'snamemapgrid', 'Search Scientific Name', choices = c("Mammalia", "Animalia", "Viruses", "Archaea", "incertae sedis",
                                                                                                         "Protozoa", "Bacteria", "Chromista", "Fungi", "Plantae"),
                                                          options = list(
                                                            placeholder = 'Please select an option below',
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                          )
                                                        ),selectizeInput("countrymapgrid", "Select Country", choices = country[2],
                                                                         options = list(
                                                                           placeholder = 'Please select Country',
                                                                           onInitialize = I('function() { this.setValue(""); }')
                                                                         )),numericInput("limitmapgrid","Enter a search limit:",value = 40,min = 10,max = 1000000),
                                                        actionButton("searchmapgrid",label="Search || Update",styleclass="primary"),hr(), radioButtons("downtype", label = "Select file type", choices = c("JPG"="jpg", "PNG"="png", "PDF"="pdf"), inline = TRUE), downloadButton(outputId = "down", label = "Download Plot"))),
                          #main panel of visualization tab
                          mainPanel(
                            tabsetPanel(id="spatialvisualization",
                              tabPanel("Mapgrid", value = 1, plotOutput("mapgrid")),
                            tabPanel("Distrigraph", value = 2)
                            )
                          )
                        )
                        ),#End of spatial visualization tab
               #Starting of Sub Tab Temporal visualization
               tabPanel("Temporal Visualizations", sidebarLayout(
                 sidebarPanel(conditionalPanel(condition = "input.temporalvisualization==1", textInput("t","qdq"))
                 ),
                 mainPanel(
                   tabsetPanel(id="temporalvisualization",
                               tabPanel("Bdcalendarheat", value = 1),
                               tabPanel("Chronohorogram", value = 2),
                               tabPanel("Tempolar", value = 3)
                   )
                 )
               )),#End of temporal visualization tab
               #Starting of Sub Tab Temporal visualization
               tabPanel("Taxonomic Visualizations", sidebarLayout(
                 sidebarPanel(conditionalPanel(condition = "input.taxonomicvisualization==1", textInput("t","qdq"))
                 ),
                 mainPanel(
                   tabsetPanel(id="taxonomicvisualization",
                               tabPanel("Taxotree", value = 1),
                               tabPanel("WordCloud", value = 2)
                   )
                 )
               ))#End of spatial visualization tab
               )#End of navbar menu called visualization
    
    )#End of navbar page
)#End of fluidpage

# Define 'server ..........................................................................................................
server <- function(input, output, session) {
  
  #For select all attritbute in gbif data
  observe({
    if("SelectAll" %in% input$fields)
      selected_choices=a[-1,] # choose all the choices _except_ "Select All"
    else
      selected_choices=input$fields # update the select input with choice selected by user
    updateSelectInput(session, "fields", selected = selected_choices)
  })

  
  
  #Download type of spatial tab
  spatialtype <- reactive({
    input$downtype
  })
  #Download gbif type of data
  fileext <- reactive({
    switch(input$dataradio,
           "Excel (CSV)" = "csv", "Text (TSV)" = "txt","Text (Space Separated)" = "txt", "Doc" = "doc")
    
  })
  
  
  #Download rinat type of data
  fileextrinat <- reactive({
    switch(input$dataradiorinat,
           "Excel (CSV)" = "csv", "Text (TSV)" = "txt","Text (Space Separated)" = "txt", "Doc" = "doc")
    
  }) 
  
  #Download function for spatial type plot
  output$down <- downloadHandler(filename = function(){
    paste("hist",spatialtype(), sep = ".")
  },
    content = function(file){
      if(spatialtype()=="pdf")
        pdf(file)
      else
        png(file)
      hist(c(1,2,3,4,5))
      dev.off()
    }
  )
  
  #Download button for Data
  output$databutton <- downloadHandler(filename = function(){
    paste(input$dataradio, fileext(), sep = ".")
  }, content = function(file){
    sep <- switch(input$dataradio, "Excel (CSV)" = ",", "Text (TSV)" = "\t","Text (Space Separated)" = " ", "Doc" = " ")
    write.table(occ <- gbif(input$sname,input$limit,input$cntry, input$fields), file, sep = sep, row.names = FALSE)
  })
  
  
  #Download button for rinat
  output$databuttonrinat <- downloadHandler(filename = function(){
    paste(input$dataradiorinat, fileextrinat(), sep = ".")
  }, content = function(file){
    sep <- switch(input$dataradiorinat, "Excel (CSV)" = ",", "Text (TSV)" = "\t","Text (Space Separated)" = " ", "Doc" = " ")
    write.table(inat<-inatdata(input$queryrinat, input$taxon_namerinat, input$maxrinat, input$year, input$month, input$date), file, sep = sep, row.names = FALSE)
  })
  
  
  observeEvent(input$searchrinat,{
    output$rinattable <- renderDataTable(inat<-inatdata(input$queryrinat, input$taxon_namerinat, input$maxrinat, input$year, input$month, input$date))
  })
  
  #Output for Summart Tab
  output$summary <- renderText("Hello this is a text")
  
  #Output for SPatial Tab
  output$hist = renderPlot(hist(c(1,2,3,4,5)))
  
  #Output for Data Tab
  observeEvent(input$search, {
    output$table = renderDataTable(occ <- gbif(input$sname,input$limit,input$cntry, input$fields))
  })
  
  observeEvent(input$searchmapgrid, {
    output$mapgrid <- renderPlot({
      mapgridfunction(input$snamemapgrid, input$limitmapgrid, input$countrymapgrid)
    })
  })
}#END OF SERVER



#functions.............................................................................
#webmap
mapgridfunction <- function(sname="Mammals", limit=40, cntry="world"){
  if(cntry=="world")
    cntry=NULL
  key <- name_backbone(name = sname)$usageKey
  mammals <-occ_search(taxonKey = key,country = cntry, limit = limit, hasCoordinate=TRUE, hasGeospatialIssue=FALSE, return = "data")
  mammals <- format_bdvis(mammals,source='rgbif')
  mammals_g <- mammals[c("Longitude", "Latitude")]
  mammals_g=format_bdvis(mammals_g,source='rgbif')
  if(is.null(cntry))
    cntry = "."
  mapgrid(indf = mammals_g, ptype = "records", title = sname, legscale = 0, collow = "blue", colhigh = "red",
          mapdatabase = "world", region = cntry, gridscale = 1 )
}

#Data Download...........................................................................

#rinat...................................................................................
inatdata <- function(queries=NULL, taxon="reptileindia", max=50, year=NULL, month=NULL, day=NULL){
  inat<-get_inat_obs(query = queries, taxon_name = taxon, taxon_id = NULL, quality = NULL, geo = TRUE, year = year, month = month, day = day, bounds = NULL, maxresults = max, meta = FALSE)
}

#gbif....................................................................................
gbif <- function(sname="Mammalia", olimit=10, cntry="world", fields){
  if(cntry=="world")
    cntry=NULL
  key <- name_backbone(name = sname)$usageKey
  occ <- occ_search(taxonKey = key,country = cntry, limit = olimit,
                    year = "2017,2018" ,return = "data")
  if(is.null(fields)){
    fields=c("class","family", "genus", "species")
  }
  else{
    fields = fields
  }
  return(occ[fields])
}

# Run the application 
shinyApp(ui = ui, server = server)

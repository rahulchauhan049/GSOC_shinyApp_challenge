# loading packages, if not installed and installing them.
pcakages <- c( "rgbif", "bdvis", "shinythemes") # list of packages needed
req_packages <- pcakages[!(pcakages %in% installed.packages()[, "Package"])] # checking if the exist
if (length(req_packages) > 0) { # installing is needed
  install.packages(req_packages, dependencies = TRUE)
}
#Display
sapply(pcakages, require, character.only = TRUE)

library(shiny)
#Download required tables from local system
a <- read.csv("a.csv");
country <- read.csv("countrycode.csv");


# Define UI for application that draws a histogram........................................................................................................
ui <- fluidPage(
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
      tabPanel("Data", value = "data", 
               sidebarLayout(
                 
                 sidebarPanel(selectizeInput(
                   'sname', 'Search Scientific Name', choices = c("Animalia", "Viruses", "Archaea", "incertae sedis",
                                                                  "Protozoa", "Bacteria", "Chromista", "Fungi", "Plantae"),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
                 ),
                 selectizeInput("country", "Select Country", choices = country[2],
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
                 )
                 ,
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   dataTableOutput('table')
                 ))

    # Sidebar with a slider input for number of bins 
    
        ),
    navbarMenu("Visualization",
               tabPanel("Spatial Visualizations",
                        sidebarLayout(
                          sidebarPanel(conditionalPanel(condition = "input.spatialvisualization==1", textInput("s","sda"))),
                          mainPanel(
                            tabsetPanel(id="spatialvisualization",
                              tabPanel("hist", value = 1, plotOutput("hist"),hr(), radioButtons("downtype", label = "Select file type", choices = c("JPG"="jpg", "PNG"="png", "PDF"="pdf"), inline = TRUE), downloadButton(outputId = "down", label = "Download Plot")),
                              tabPanel("map", value = 2),
                            tabPanel("third", value = 3)
                            )
                          )
                        )
                        ),
               tabPanel("Temporal Visualizations", value = 4),
               tabPanel("Taxonomic Visualizations", value = 5))
    
    )
)

# Define 'server logic required to draw a histogram..........................................................................................................
server <- function(input, output, session) {
  
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
  #Download type of data
  fileext <- reactive({
    switch(input$dataradio,
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
  
  #Output for Summart Tab
  output$summary <- renderText("Hello this is a text")
  #Output for SPatial Tab
  output$hist = renderPlot(hist(c(1,2,3,4,5)))
  #Output for Data Tab
  observeEvent(input$search, {
    output$table = renderDataTable(occ <- gbif(input$sname,input$limit,input$cntry, input$fields))

  })
}



#functions.............................................................................
gbif <- function(sname="Mammalia", olimit=10, cntry="US", fields){
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

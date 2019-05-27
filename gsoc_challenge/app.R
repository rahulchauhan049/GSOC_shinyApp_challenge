# loading packages, if not installed and installing them.

pcakages <- c( "rgbif", "bdvis") # list of packages needed
req_packages <- pcakages[!(pcakages %in% installed.packages()[, "Package"])] # checking if the exist
if (length(req_packages) > 0) { # installing is needed
  install.packages(req_packages, dependencies = TRUE)
}
#Display
sapply(pcakages, require, character.only = TRUE)
library(shiny)
a <- read.csv("a.csv");
country <- read.csv("countrycode.csv");
for(i in country){
  choice = paste0('"',country[i,1],'"','=','"',country[i,2],'"')
}
choice

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GSOC Shiny Challenge"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectizeInput(
            'sname', 'Search Scientific Name', choices = c("Animalia", "Viruses", "Archaea", "incertae sedis",
                                                           "Protozoa", "Bacteria", "Chromista", "Fungi", "Plantae"),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          ),
          selectizeInput("country", "Select Country", choices = as.data.frame(choice),
                         options = list(
                           placeholder = 'Please select Country',
                           onInitialize = I('function() { this.setValue(""); }')
                         )),
          selectizeInput(
            'fields', 'Select Attributes to be displayed', choices = a , multiple = TRUE
          ),
          numericInput("limit","Enter a search limit:",value = 10,min = 10,max = 1000000),
          actionButton("search",label="Search || Update",styleclass="primary")            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput('table')
        )
    )
)

# Define 'server logic required to draw a histogram
server <- function(input, output) {
  gbif <- function(sname="Mammalia", olimit=10, cntry="US"){
    key <- name_backbone(name = sname)$usageKey
    occ <- occ_search(taxonKey = key,country = input$country, limit = olimit,
                      year = "2017,2018" ,return = "data")
    return(occ)
  }

  
  
  # occ <- occ[c("class","family", "genus", "species", "specificEpithet", "stateProvince")]
  output$table = renderDataTable(occ <- gbif(input$sname,input$limit))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

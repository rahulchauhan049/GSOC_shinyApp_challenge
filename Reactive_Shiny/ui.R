#Importing packages
library("DT")
library("rgbif")
library("bdvis")
library("rinat")
library("shinythemes")
library("shiny")
library("leaflet")
library("crosstalk")
library("plotly")
library("r2d3")
library("dplyr")
library("collapsibleTree")
library("shinycssloaders")
library("sqldf")
library("plotrix")
library("highcharter")
library("tidyr")
library("ggplot2")
library(nycflights13)
library(ggstat)



# Define UI for application
columnName <- read.csv("www/csv/columnNames.csv")
country <- read.csv("www/csv/countrycode.csv")


shinyUI(fluidPage(
  #GSOC Image
  # Application title
  titlePanel("Reactive Shiny Experiment"),
  theme = shinytheme("flatly"),
  # returns URL of a shiny theme
  # themeSelector(),
  navbarPage(
    title = "Google Summer of code 2019",
    id = "nav",
    tabPanel(
      "Summary",
      value = "Summary",
      tags$div(
        id = "profile",
        style = "
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
        p(
          "This shiny package allows you to use functioning of bdvis package in an interactive manner. This shiny app allows you to download dataset from gbif.You can select various parameters through graphical pannels. "
        ),
        hr(),
        h4("Visualizations"),
        p(
          "This shiny also allows you to use various visualization functions on the dataset that you have downloaded"
        ),
        hr(),
        p("To start, click in 'data' tab on the top navigation bar")
        
      )#End of div
    ),
    #Data Tab starts here..............................................................
    tabPanel("Data", value = "data",
             sidebarLayout(
               #Sidebar panel of data tab......
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.datatabs==1",
                   selectInput(
                     "datasetselected",
                     "Choose from existing dataset",
                     c("Mammals" = "sampledata.csv", "Hyena" = "hyenaData.csv")
                   ),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Checkbox if file has header ----
                   checkboxInput("header1", "Header", TRUE),
                   
                   # Input: Select separator ----
                   radioButtons(
                     "sep1",
                     "Separator",
                     choices = c(
                       Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"
                     ),
                     selected = ","
                   ),
                   
                   # Input: Select quotes ----
                   radioButtons(
                     "quote1",
                     "Quote",
                     choices = c(
                       None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"'
                   ),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Select number of rows to display ----
                   radioButtons(
                     "disp1",
                     "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.datatabs==2",
                   fileInput(
                     "file1",
                     "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                   ),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Checkbox if file has header ----
                   checkboxInput("header", "Header", TRUE),
                   
                   # Input: Select separator ----
                   radioButtons(
                     "sep",
                     "Separator",
                     choices = c(
                       Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"
                     ),
                     selected = ","
                   ),
                   
                   # Input: Select quotes ----
                   radioButtons(
                     "quote",
                     "Quote",
                     choices = c(
                       None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"'
                   ),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Select number of rows to display ----
                   radioButtons(
                     "disp",
                     "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.datatabs==3",
                   selectizeInput(
                     'sname',
                     'Search Scientific Name',
                     choices = c(
                       "Mammalia",
                       "Animalia",
                       "Viruses",
                       "Archaea",
                       "incertae sedis",
                       "Protozoa",
                       "Bacteria",
                       "Chromista",
                       "Fungi",
                       "Plantae"
                     ),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   ),
                   selectizeInput(
                     "cntry",
                     "Select Country",
                     choices = country[2],
                     options = list(
                       placeholder = 'Please select Country',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   ),
                   selectizeInput(
                     'fields',
                     'Select Attributes to be displayed',
                     choices = columnName ,
                     multiple = TRUE
                   ),
                   numericInput(
                     "limit",
                     "Enter a search limit:",
                     value = 10,
                     min = 10,
                     max = 1000000
                   ),
                   actionButton("search", label = "Search || Update", styleclass =
                                  "primary"),
                   hr(),
                   "Click on the download button to download dataset observation",
                   radioButtons(
                     "dataradio",
                     label = "Select file type",
                     choices = c("Excel (CSV)", "Text (TSV)", "Text (Space Separated)", "Doc"),
                     inline = TRUE
                   ),
                   downloadButton(outputId = "databutton", label = "Download Data")
                   
                 ),
                 conditionalPanel(
                   condition = "input.datatabs==4",
                   textInput("queryrinat", "Query"),
                   textInput("taxon_namerinat", "taxon_name"),
                   numericInput("maxrinat", "No. of Obervations", value = 50),
                   numericInput("year", "Year", value = 2019),
                   numericInput(
                     "month",
                     "Month",
                     value = 01,
                     min = 01,
                     max = 12
                   ),
                   numericInput(
                     "date",
                     "Date",
                     value = 01,
                     min = 01,
                     max = 30
                   ),
                   actionButton("searchrinat", label =
                                  "Search || Update", styleclass = "primary"),
                   hr(),
                   "Click on the download button to download dataset observation",
                   radioButtons(
                     "dataradiorinat",
                     label = "Select file type",
                     choices = c("Excel (CSV)", "Text (TSV)", "Text (Space Separated)", "Doc"),
                     inline = TRUE
                   ),
                   downloadButton(outputId = "databuttonrinat", label = "Download Data")
                 )
               ),
               
               
               
               #Main panel of data tab..........
               mainPanel(
                 tabsetPanel(
                   id = "datatabs",
                   tabPanel(
                     "Existing Dataset",
                     value = 1,
                     DT::dataTableOutput("tableoutput")
                   ),
                   tabPanel("Upload Dataset", value = 2, DT::dataTableOutput("tableupload")),
                   tabPanel("GBIF", value = 3, dataTableOutput('table')),
                   tabPanel("RINAT", value = 4, dataTableOutput('rinattable'))
                 )
               )
             )),
    tabPanel(
      "Visualization",
      value = "visualization",
      fluidPage(
        fluidRow(
          column(
            width = 6,
            style = 'padding: 5px',
            leafletOutput("mymap") %>% withSpinner(color = "#0dc5c1")
          ),
          column(
            width = 6,
            style = 'padding: 5px',
            selectInput(
              "pieselect",
              "Select Column to be displayed",
              c("Kingdom", "Phylum", "Order", "Family", "Genus", "Species"),
              selected = "Order"
            )  ,
            plotlyOutput("pie") %>% withSpinner(color = "#0dc5c1")
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = 'padding: 5px',
            selectInput(
              "barselect",
              "Select Column to be displayed",
              c("Kingdom", "Phylum", "Order", "Family", "Genus", "Species"),
              selected = "Order"
            ),
            plotlyOutput("bar") %>% withSpinner(color = "#0dc5c1")
          ),
          column(
            width = 6,
            style = 'padding: 5px',
            selectInput(
              "tree",
              "Select Hierarchy",
              c("kingdom", "phylum", "order", "family", "genus"),
              selected = c("order", "family", "genus"),
              multiple = TRUE,
              selectize = TRUE
            ),
            collapsibleTreeOutput("tree")
          )
        ),
        DT::dataTableOutput("tb")
      )
    ),
    tabPanel(
      "Temporal visualizations",
      value = "page4",
      fluidPage(
        fluidRow(
          column(
            width = 6,
            style = "padding: 5px",
            selectizeInput(
              "temporaldataset",
              "Select Dataset",
              c( "Mammals" = "mammalsLarge.csv", "Hyena" = "hyenaData.csv")
            )
          ),
          column(
            width = 6,
            style = 'padding: 5px',
            selectizeInput(
              "temporalcolumn",
              "Select Column",
              c(
                "Order" = "order",
                "Family" = "family",
                "Genus" = "genus",
                "Species" = "species"
              )
            )
          )
        ),
        fluidRow(
          column(width = 6, styple = 'padding: 5px', plotOutput("timebars")),
          column(width = 6, style = 'padding: 5px', plotOutput("timerose"))
        ),
        fluidRow(
          column(width = 4, style = 'padding: 5px', plotOutput("monthrose")),
          column(
            width = 4,
            style = 'padding: 5px',
            plotOutput("monthtimebars")
          ),
          column(
            width = 4,
            style = 'padding: 5px',
            plotOutput("monthtimerose")
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = 'padding: 5px',
            plotlyOutput("yearlines")
          ),
          column(
            width = 6,
            style = 'padding: 5px',
            plotlyOutput("yearlines3d")
          )
        )
      )
    ),
    tabPanel(
      "Practice for reactive",
      value = "page5",
      fluidPage(fluidRow(
        column(width = 6, style = 'padding: 5px', plotlyOutput("arr_time")),
        column(width = 6, style = 'padding: 5px', plotlyOutput("dep_time"))
      ),
      fluidRow(
        column(width = 6, style = 'padding: 5px', plotlyOutput("fifthpie")),
        column(width = 6, style = 'padding: 5px', leafletOutput("fifthmap")),
        verbatimTextOutput("fifthtext")
      ),
      fluidRow(
        column(width = 6, style = 'padding: 5px', plotlyOutput("fifthmonth")),
        column(width = 6, style = 'padding: 5px', plotlyOutput("fifthday")),
        tableOutput("fifthtext2")
      ))
    )
  )
))#End of navbar menu called visualization)#End of navbar page)#End of fluidpage)
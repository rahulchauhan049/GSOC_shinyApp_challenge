#Importing libraries
library("bdvis")
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("r2d3")
library("dplyr")
library("rgbif")
library("jsonlite")
#Import Datasets
data<- read.csv("data.csv")


#Shiny App starts from here............................
ui <-  dashboardPage(
  
  title = "Interactive and reactive Shiny app experiment", skin = "purple",
  dashboardHeader(title = "Interactive and reactive Shiny app experiment"),
  
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuSubItem("Data Visualization", tabName = "plots")
      
    )
  ),
  
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "data",
              fluidRow(h1("Records from gbif."),
                       br(), h3("Some recoreds with globally spread occurance."), br(), br()),
              fluidRow(tabPanel(title = "Data",status = "primary",solidHeader = T, dataTableOutput('table'), background = "aqua"))
      ),
      
      tabItem(tabName = "plots",
              fluidRow(
                column(1,
              verbatimTextOutput("selected")),
              ),fluidRow(
              d3Output("d3"),
              d3Output("map")
              )
                  )
      
    )
  )
)


# server
server <- function(input, output){
  output$table = renderDataTable(data)
  #Here i have called shiny module that i made to create word cloud.....
  #callModue(wordOutput, "same name that used in UI part")
  output$d3 <- renderD3({
    r2d3(
      data = read.csv("data1.csv"),
      css = "circlepacking.css", d3_version = 4, script = "circlepacking.js"
    )
  })
  
  observeEvent(input$bar_clicked,{
    output$map <- renderD3(d3_map(data, name = input$bar_clicked))
  })
  
  output$map <- renderD3(
    d3_map(data)
  )
}


#Functions.................................................
hierarchy <- function(data) {
  data <- na.omit(data[c("kingdom", "phylum", "order", "family")])
  data <- arrange(data, order)
  temp <- as.data.frame(table(data["family"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "family", by.y = "Var1")
  temp <- temp[c("kingdom", "phylum", "order", "family", "Freq")]
  id <-
    as.data.frame(paste(data$kingdom, data$phylum, data$order, data$family, sep =
                          "."))
  names(id)[1] <- "id"
  id <- arrange(id, id)
  temp <- arrange(temp, order)
  id <- cbind(id, temp["Freq"])
  
  for (i in na.omit(unique(data["kingdom"]))) {
    a <- as.data.frame(paste(i))
    for (j in na.omit(unique(data["phylum"]))) {
      b <- as.data.frame(paste(i, j, sep = "."))
      for (k in na.omit(unique(data["order"]))) {
        c <- as.data.frame(paste(i, j, k, sep = "."))
      }
    }
  }
  a <- cbind(a, NA)
  b <- cbind(b, NA)
  c <- cbind(c, NA)
  names(a)[1] <- paste("id")
  names(a)[2] <- paste("value")
  names(b)[1] <- paste("id")
  names(b)[2] <- paste("value")
  names(c)[1] <- paste("id")
  names(c)[2] <- paste("value")
  names(id)[2] <- paste("value")
  return(rbind(a, b, c, id))
}
d3_map <- function(data, map="world", name=NA){
  data<- data[c("decimalLatitude", "decimalLongitude", "order")]
  data<- data<-na.omit(data)
  
  if(is.na(name)){
  }else{
    clicked <- tail(unlist(strsplit(name, "\\.")),n=1)
    data <- data %>% filter(order == clicked)
  }
  names(data)[names(data) == "order"] <- "name"
  modified <- ""
  modified <- list(
    traits = colnames(data),
    values = data
  )
  modified<-jsonlite::toJSON(modified)
  if(map=="world"){
    r2d3::r2d3(data = c(jsonlite::read_json("assets/json/worldmap.json"),jsonlite::parse_json(modified)),
               css = "assets/css/map.css",
               d3_version = 3,
               dependencies = c("assets/js/topojson.min.js",
                                "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"),
               script = "assets/js/worldmap.js")
  }
}

shinyApp(ui, server)
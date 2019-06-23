library(shiny)
library(r2d3)
library("dplyr")
library("rgbif")
library("jsonlite")
data<- read.csv("data.csv")
ui <- fluidPage(
  verbatimTextOutput("selected"),
  d3Output("d3"),
  d3Output("bar"),
  d3Output("map")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(
      data = read.csv("data1.csv"),
      css = "circlepacking.css", d3_version = 4, script = "circlepacking.js"
    )
  })
  observeEvent(input$bar_clicked, {
    output$selected <- renderText(input$bar_clicked)
    output$map <- renderD3(
      d3_map(data = read.csv("data.csv"),name=input$bar_clicked)
    )
    
    
  }, ignoreInit = TRUE )
# output$map <- renderD3(
#   d3_map(data = read.csv("data.csv"))
# )
  
  
}
#functions
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
    data <- data %>% filter(order ==clicked)
  }
  names(data)[names(data) == "order"] <- "name"
  modified <- list(
    traits = colnames(data),
    values = data
  )
  write_json(modified, "file.json")
  if(map=="world"){
    r2d3::r2d3(data = c(jsonlite::read_json("assets/json/worldmap.json"),jsonlite::read_json("file.json")),
               css = "assets/css/map.css",
               d3_version = 3,
               dependencies = c("assets/js/topojson.min.js",
                                "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"),
               script = "assets/js/worldmap.js")
  }
}
shinyApp(ui = ui, server = server)


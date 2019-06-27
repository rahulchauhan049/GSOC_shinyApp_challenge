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
data<- read.csv("../data/sampledata.csv")
data1<- read.csv("../data/hyenaData.csv")

#Shiny App starts from here...............................................
ui <-  dashboardPage(
  title = "Interactive and reactive Shiny app experiment",
  skin = "purple",
  dashboardHeader(title = "Interactive and reactive Shiny app experiment"),
  
  
  
  dashboardSidebar(sidebarMenu(
    selectInput("selectdata", "Select DataSet:",
                c("Hyena Dataset" = "hyenaData.csv",
                  "Mammals Dataset" = "sampledata.csv")),
    
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Reactive Visualization Experiment", tabName = "plots"),
    menuItem("Static Visualizations", menuSubItem("bubbleplot", tabName = "bubbleplot"),
             menuSubItem("circlepacking", tabName = "circlepacking"),
             menuSubItem("Dendogram", tabName = "dendogram"),
             menuSubItem("Map", tabName = "map"),
             menuSubItem("RadialTree", tabName = "radialtree"),
             menuSubItem("Treemap", tabName = "treemap")),
    menuItem("Interactive Visualizations",
             menuSubItem("Dendogram", tabName = "idendogram"),
             menuSubItem("circlepacking", tabName = "icirclepack"),
             menuSubItem("Barchart", tabName = "ibarchart")
    
  ))),
  
  
  dashboardBody(tabItems(
    tabItem(tabName = "data",
            fluidRow(
              h1("Records from gbif."),
              br(),
              h3("Some recoreds with globally spread occurance."),
              br(),
              br()
            ),
            fluidRow(
              tabPanel(
                title = "Data",
                status = "primary",
                solidHeader = T,
                dataTableOutput('table'),
                background = "aqua"
              )
            )),
    
    tabItem(
      tabName = "plots",
      fluidRow(verbatimTextOutput("selected"),
               d3Output("d3")),
      plotOutput("map")
      
    ),
    tabItem(tabName = "bubbleplot",d3Output("bubble")),
    tabItem(tabName = "circlepacking",d3Output("circle")),
    tabItem(tabName = "dendogram",d3Output("dendo")),
    tabItem(tabName = "map",d3Output("mapoutput")),
    tabItem(tabName = "radialtree",d3Output("radial")),
    tabItem(tabName = "treemap",d3Output("tree")),
    tabItem(tabName = "idendogram", d3Output("interactivedendogram")),
    tabItem(tabName = "icirclepack", d3Output("interactivecirclepack")),
    tabItem(tabName = "ibarchart", d3Output("interactivebarchart"))
    
    
  ))
)


# server
server <- function(input, output) {
  data<- reactive(read.csv(paste0("../data/",input$selectdata, sep=""))
)
  output$table = renderDataTable(data())
  #Here i have called shiny module that i made to create word cloud.....
  #callModue(wordOutput, "same name that used in UI part")
  output$d3 <- renderD3({
    r2d3(
      data = hierarchy(data()),
      css = "circlepacking.css",
      d3_version = 4,
      script = "circlepacking.js"
    )
  })
  
  observeEvent(input$bar_clicked, {
    output$selected <- renderText(input$bar_clicked)
    output$map <- renderPlot({
      {data<- data()
        data <- format_bdvis(data,source='rgbif')
        data1 <- data[c("Longitude", "Latitude", "order")]
        clicked <- unlist(strsplit(input$bar_clicked, "\\."))
        if(length(clicked)>2){
        clicked <- tail(clicked, n=1)
        data1 <- data1 %>% filter(order == clicked)
        }
        data1 <- format_bdvis(data1,source='rgbif')
        mapgrid(indf = data1, ptype = "records", legscale = 0, collow = "blue", colhigh = "red",
                mapdatabase = "world", gridscale = 1 )
        
      }
    })
  })
  
  output$map <- renderPlot({
    data <- format_bdvis(data(),source='rgbif')
    data1 <- data[c("Longitude", "Latitude")]
    data1 <- format_bdvis(data1,source='rgbif')
    mapgrid(indf = data1, ptype = "records", title = "Mammals", legscale = 0, collow = "blue", colhigh = "red",
            mapdatabase = "world", gridscale = 1)
    
  })
  
  #bubble
  output$bubble <- renderD3({data<- data()
    data <- as.data.frame(table(data["genus"]))
    names(data)[1]<-paste("id") 
    names(data)[2]<-paste("value")
    r2d3(data = data, d3_version = 4, script = "../r2d3/bubbleplot/assets/js/bubble.js")
  })
  
  #circle
  output$circle <- renderD3(
  r2d3(data = hierarchy(data()),css = "../r2d3/circlepacking/assets/css/circlepacking.css", d3_version = 4, script = "../r2d3/circlepacking/assets/js/circlepacking.js")
  )
  
  #dendo
  output$dendo <- renderD3(r2d3(
    data = hierarchy(data()),
    css = "../r2d3/dendogram/assets/css/dendogram.css",
    d3_version = 4,
    script = "../r2d3/dendogram/assets/js/dendogram.js"
  ))
  
  #mapoutput
  output$mapoutput <- renderD3({d3_map <- function(data, map = "world") {
    data<- data[c("decimalLatitude", "decimalLongitude", "genericName")]
    data<- data<-na.omit(data)
    names(data)[names(data) == "genericName"] <- "name"
    modified <- list(
      traits = colnames(data),
      values = data
    )
    modified<-jsonlite::toJSON(modified)
    if(map=="world"){
      r2d3::r2d3(data = c(jsonlite::read_json("../r2d3/map/assets/json/worldmap.json"),jsonlite::parse_json(modified)),
                 css = "../r2d3/map/assets/css/map.css",
                 d3_version = 3,
                 dependencies = c("../r2d3/map/assets/js/topojson.min.js",
                                  "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"),
                 script = "../r2d3/map/assets/js/worldmap.js")
    }else if(map=="IN"){
      r2d3::r2d3(data = c(jsonlite::read_json("../r2d3/map/assets/json/india.json"),jsonlite::parse_json(modified)),
                 css = "../r2d3/map/assets/css/indiamap.css",
                 d3_version = 3,
                 dependencies = c("../r2d3/map/assets/js/topojson.min.js",
                                  "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"
                 ),
                 script = "../r2d3/map/assets/js/indiamap.js")
    }
  }
  #Example of India Data
  #indiadata<- read.csv("../data/indiadata.csv")
  data<- data()
  d3_map(data)
  })
  
  #radialtree
  output$radial <- renderD3(r2d3(data = hierarchy(data()),
                                 css = "../r2d3/radialtree/assets/css/radialtree.css",
                                 d3_version = 4,
                                 script = "../r2d3/radialtree/assets/js/radialtree.js"))
  
  #treemap
  output$tree <- renderD3(r2d3(data = hierarchy(data()),css = "../r2d3/treemap/assets/css/treemap.css", d3_version = 4, script = "../r2d3/treemap/assets/js/treemap.js")
)
  
  #Interactive Visualizations..........................................................
  #dendogram
  output$interactivedendogram <- renderD3({
    r2d3(data=hierarchy(data()),css="../r2d3/interactive/dendogram/src/css/dendogram.css", d3_version = 4, script = "../r2d3/interactive/dendogram/src/js/dendogram.js")
    })
  
  #circlepack
  output$interactivecirclepack <- renderD3(r2d3(data = hierarchy(data()),css = "../r2d3/interactive/circlepack/src/css/circlepacking.css", d3_version = 4, script = "../r2d3/interactive/circlepack/src/js/circlepacking.js")
)
  
  #Barchart
  output$interactivebarchart <- renderD3(r2d3(data=jsonlite::read_json("../data/sampledata.json"),css="../r2d3/interactive/barchart/src/css/barchart.css", d3_version = 3 , script = "../r2d3/interactive/barchart/src/js/barchart.js")
)
}


#Functions.................................................
hierarchy <- function(data) {
  data <- na.omit(data[c("phylum", "order", "family", "genus")])
  data <- arrange(data, family)
  temp <- as.data.frame(table(data["genus"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
  temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
  id <-
    as.data.frame(paste(data$phylum, data$order, data$family, data$genus, sep =
                          "."))
  names(id)[1] <- "id"
  id <- arrange(id, id)
  temp <- arrange(temp, order)
  id <- cbind(id, temp["Freq"])
  
  idnames <- id["id"]
  emptyvectora <- c()
  emptyvectorb <- c()
  emptyvectorc <- c()
  for (i in 1:nrow(idnames)) {
    s <- ((unlist(strsplit(
      as.character(idnames[i, ]), "\\."
    ))))
    s <- s[-length(s)]
    s <- as.data.frame(t(s))
    
    p <-
      as.data.frame(paste(s$V1, s$V2, s$V3, sep =
                            "."))
    t <- as.data.frame(paste(s$V1, s$V2, sep =
                               "."))
    u <- as.data.frame(paste(s$V1, sep =
                               "."))
    emptyvectora <- append(emptyvectora, p)
    emptyvectorb <- append(emptyvectorb, t)
    emptyvectorc <- append(emptyvectorc, u)
    
  }
  emptyvectora <- as.data.frame((emptyvectora))
  emptyvectora <- unique(t(emptyvectora))
  rownames(emptyvectora) <- NULL
  emptyvectora <- as.data.frame(emptyvectora)
  emptyvectorb <- as.data.frame((emptyvectorb))
  emptyvectorb <- unique(t(emptyvectorb))
  rownames(emptyvectorb) <- NULL
  emptyvectorb <- as.data.frame(emptyvectorb)
  
  emptyvectorc <- as.data.frame((emptyvectorc))
  emptyvectorc <- unique(t(emptyvectorc))
  rownames(emptyvectorc) <- NULL
  emptyvectorc <- as.data.frame(emptyvectorc)
  
  mergeddf <- rbind(emptyvectora, emptyvectorb, emptyvectorc)
  mergeddf <- cbind(mergeddf, NA)
  
  colnames(mergeddf) <- c("id", "value")
  names(id)[2] <- paste("value")
  temp <- rbind(mergeddf, id)
  
  
  return(temp)
}

d3_map <- function(data, map = "world", name = NA) {
  data <- data[c("decimalLatitude", "decimalLongitude", "order")]
  data <- data <- na.omit(data)
  
  if (is.na(name)) {
    
  } else{
    clicked <- tail(unlist(strsplit(name, "\\.")), n = 1)
    data <- data %>% filter(order == clicked)
  }
  names(data)[names(data) == "order"] <- "name"
  modified <- ""
  modified <- list(traits = colnames(data),
                   values = data)
  modified <- jsonlite::toJSON(modified)
  if (map == "world") {
    r2d3::r2d3(
      data = c(
        jsonlite::read_json("assets/json/worldmap.json"),
        jsonlite::parse_json(modified)
      ),
      css = "assets/css/map.css",
      d3_version = 3,
      dependencies = c(
        "assets/js/topojson.min.js",
        "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"
      ),
      script = "assets/js/worldmap.js"
    )
  }
}

  
shinyApp(ui, server)

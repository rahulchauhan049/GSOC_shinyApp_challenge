# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
suppressPackageStartupMessages(library(circlepackeR))
suppressPackageStartupMessages(library(collapsibleTree))
dataset <- read.csv("smallData.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Google Trend Index"),
                collapsibleTreeOutput("ac")
                # circlepackeROutput("as")
                )


# Define server function
server <- function(input, output) {
  output$as <- renderCirclepackeR({
    dataforCircle <- formatData(dataset)
    dataforCircle$pathString <-
      paste(
        "Vis",
        dataforCircle$group,
        dataforCircle$subgroup,
        dataforCircle$subsubgroup,
        sep = "/"
      )
    population <- as.Node(dataforCircle)
    # Make the plot
    circlepackeR(population, size = "value")
  })
  
  output$ac <- renderCollapsibleTree({
    temp <- formatData(dataset)
    temp %>%
      group_by(root, group, subgroup, subsubgroup) %>%
      summarize(`Freq` = n()) %>%
      collapsibleTreeSummary(
        hierarchy = c("group", "subgroup", "subsubgroup"),
        root = "Geography",
        width = "100%",
        attribute = "Freq",
        zoomable = FALSE
      )
  })
}
formatData <- function(data) {
  data <- na.omit(data[c("phylum", "order", "family", "genus")])
  if (!nrow(data[-which(data[, "phylum"] == ""), ]) == 0) {
    data <- data[-which(data[, "phylum"] == ""), ]
  }
  if (!nrow(data[-which(data[, "order"] == ""), ]) == 0) {
    data <- data[-which(data[, "order"] == ""), ]
  }
  if (!nrow(data[-which(data[, "family"] == ""), ]) == 0) {
    data <- data[-which(data[, "family"] == ""), ]
  }
  if (!nrow(data[-which(data[, "genus"] == ""), ]) == 0) {
    data <- data[-which(data[, "genus"] == ""), ]
  }
  data <- arrange(data, family)
  temp <- as.data.frame(table(data["genus"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
  temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
  colnames(temp) <-
    c("root", "group", "subgroup", "subsubgroup", "value")
  
  return(temp)
}

shinyApp(ui = ui, server = server)
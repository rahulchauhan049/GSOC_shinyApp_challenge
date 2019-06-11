library("r2d3")
bubble <- function(data, field = "family"){
  data <- as.data.frame(table(data[field]))
  names(data)[1]<-paste("id") 
  names(data)[2]<-paste("value")
  r2d3(data = data, d3_version = 4, script = "assets/js/bubble.js")
}

data<- read.csv("assets/csv/data.csv")
bubble(data, field = "genus")

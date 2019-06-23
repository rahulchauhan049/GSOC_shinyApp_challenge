library(jsonlite)
library("r2d3")
d3_map <- function(data, map = "world") {
  data<- data[c("decimalLatitude", "decimalLongitude", "genericName")]
  data<- data<-na.omit(data)
  names(data)[names(data) == "genericName"] <- "name"
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
  }else if(map=="IN"){
    r2d3::r2d3(data = c(jsonlite::read_json("assets/json/india.json"),jsonlite::parse_json(modified)),
               css = "assets/css/indiamap.css",
               d3_version = 3,
               dependencies = c("assets/js/topojson.min.js",
                                "http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js"
                                ),
               script = "assets/js/indiamap.js")
  }
}
#Example using world Data
data<- read.csv("assets/csv/worlddata.csv")

#Example Using India Data
data<- read.csv("assets/csv/indiadata.csv")

d3_map(data)


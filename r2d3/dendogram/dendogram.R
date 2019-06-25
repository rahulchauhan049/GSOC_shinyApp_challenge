library("r2d3")
library("dplyr")
dendogram <- function(data, source = "na") {
  data <- data
  if (source == "rgbif") {
data <- na.omit(data[c("phylum", "order", "genus")])
    data <- arrange(data, order)
    data <- unique(data)
    id <-
      as.data.frame(paste(data$phylum, data$order, data$genus, sep =
                            "."))
    names(id)[1] <- "id"
    phylum <- na.omit(unique(data["phylum"]))
    order <- na.omit(unique(data["order"]))
    genus <- na.omit(unique(data["genus"]))
    for (i in phylum) {
      a <- paste(i)
      for (j in order) {
        b <- paste(i, j, sep = ".")
        for (k in genus) {
          c <- paste(i, j, k, sep = ".")
          # for (l in family) {
          #   d <- paste(i, j, k, l, sep = ".")
          #   # for(m in genus){
          #   #   e<- paste(i,j,k,l,m,sep = ".")
          #   # }
          # }
        }
      }
    }
    a <- as.data.frame(a)
    b <- as.data.frame(b)
    c <- as.data.frame(c)
    names(a)[1] <- paste("id")
    names(b)[1] <- paste("id")
    names(c)[1] <- paste("id")
    new <- rbind(a, b, c, id)
    r2d3(
      data = new,
      css = "assets/css/dendogram.css",
      d3_version = 4,
      script = "assets/js/dendogram.js"
    )
  } else {
    r2d3(
      data = data,
      css = "assets/css/dendogram.css",
      d3_version = 4,
      script = "assets/js/dendogram.js"
    )
  }
}

flare <- read.csv("../../data/flare.csv")
data <- read.csv("../../data/sampledata.csv")
data<- read.csv("../../data/hyenaData.csv")

dendogram(data = flare)
dendogram(data = data, source = "rgbif")

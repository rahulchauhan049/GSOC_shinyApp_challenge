library("r2d3")
library("dplyr")

hierarchy <- function(data) {
  data <- na.omit(data[c("phylum", "order", "family")])
  data <- arrange(data, order)
  temp <- as.data.frame(table(data["family"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "family", by.y = "Var1")
  temp <- temp[c("phylum", "order", "family", "Freq")]
  id <-
    as.data.frame(paste(data$phylum, data$order, data$family, sep =
                          "."))
  names(id)[1] <- "id"
  id <- arrange(id, id)
  temp <- arrange(temp, order)
  id <- cbind(id, temp["Freq"])
  

    for (j in na.omit(unique(data["phylum"]))) {
      b <- as.data.frame(paste(j, sep = "."))
      for (k in na.omit(unique(data["order"]))) {
        c <- as.data.frame(paste(j, k, sep = "."))
      }
    }
  b <- cbind(b, NA)
  c <- cbind(c, NA)

  names(b)[1] <- paste("id")
  names(b)[2] <- paste("value")
  names(c)[1] <- paste("id")
  names(c)[2] <- paste("value")
  names(id)[2] <- paste("value")
  return(rbind(b, c, id))
}
circlepack <- function(data){
  r2d3(data = hierarchy(data),css = "src/css/circlepacking.css", d3_version = 4, script = "src/js/circlepacking.js")
}

circlepack(read.csv("../../../data/sampledata.csv"))



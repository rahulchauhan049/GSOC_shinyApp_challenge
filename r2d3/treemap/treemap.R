library("r2d3")
hierarchy <- function(data) {
  data <- na.omit(data[c("order", "family" ,"genus")])
  data <- arrange(data, order)
  temp <- as.data.frame(table(data["genus"]))
  data <- unique(data)
  temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
  temp <- temp[c("order", "family","genus", "Freq")]
  id <-
    as.data.frame(paste(data$order, data$family, data$genus, sep =
                          "."))
  names(id)[1] <- "id"
  id <- arrange(id, id)
  temp <- arrange(temp, order)
  id <- cbind(id, temp["Freq"])
  
  for (i in na.omit(unique(data["order"]))) {
    a <- as.data.frame(paste(i))
    for (j in na.omit(unique(data["family"]))) {
      b <- as.data.frame(paste(i, j, sep = "."))
      for (k in na.omit(unique(data["genus"]))) {
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
temp<-hierarchy(read.csv("../../data/hyenaData.csv"))

r2d3(data = temp,css = "assets/css/treemap.css", d3_version = 4, script = "assets/js/treemap.js")

library("dplyr")

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
temp<-hierarchy(read.csv("data.csv"))

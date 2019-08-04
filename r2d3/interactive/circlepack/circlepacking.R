library("r2d3")
library("dplyr")

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

circlepack <- function(data){
  r2d3(data = hierarchy(data),css = "src/css/circlepacking.css", d3_version = 4, script = "src/js/circlepacking.js")
}

circlepack(read.csv("../../../data/hyenaData.csv"))

# data <-read.csv("../../../data/hyenaData.csv")

library("r2d3")
library("dplyr")
dendogram <- function(data, source = "na") {
  data <- data
  if (source == "rgbif") {
    #.................................................................
    # data <- data[c("kingdom", "phylum", "order", "family", "genus")]
    # kingdom <- na.omit(unique(data["kingdom"]))
    # phylum <- na.omit(unique(data["phylum"]))
    # order <- na.omit(unique(data["order"]))
    # family <- na.omit(unique(data["family"]))
    # # genus <- na.omit(unique(data["genus"]))
    #
    # for (i in kingdom) {
    #   a <- paste(i)
    #   for (j in phylum) {
    #     b <- paste(i, j, sep = ".")
    #     for (k in order) {
    #       c <- paste(i, j, k, sep = ".")
    #       for (l in family) {
    #         d <- paste(i, j, k, l, sep = ".")
    #         # for(m in genus){
    #         #   e<- paste(i,j,k,l,m,sep = ".")
    #         # }
    #       }
    #     }
    #   }
    # }
    # a <- as.data.frame(a)
    # b <- as.data.frame(b)
    # c <- as.data.frame(c)
    # d <- as.data.frame(d)
    # # e <- as.data.frame(e)
    # names(a)[1] <- paste("id")
    # names(b)[1] <- paste("id")
    # names(c)[1] <- paste("id")
    # names(d)[1] <- paste("id")
    # # names(e)[1] <- paste("id")
    # # names(df_sorted_names_asc)[1] <- paste("id")
    # new <- rbind(a, b, c, d)
    # r2d3(data = new,css = "assets/css/dendogram.css",
    #      d3_version = 4,
    #      script = "assets/js/dendogram.js")
    #................................................................................
    data <- na.omit(data[c("kingdom", "phylum", "order", "family")])
    data <- arrange(data, order)
    data <- unique(data)
    id <-
      as.data.frame(paste(data$kingdom, data$phylum, data$order, data$family, sep =
                            "."))
    names(id)[1] <- "id"
    kingdom <- na.omit(unique(data["kingdom"]))
    phylum <- na.omit(unique(data["phylum"]))
    order <- na.omit(unique(data["order"]))
    for (i in kingdom) {
      a <- paste(i)
      for (j in phylum) {
        b <- paste(i, j, sep = ".")
        for (k in order) {
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

flare <- read.csv("assets/csv/flare.csv")
data <- read.csv("assets/csv/data.csv")

dendogram(data = flare)
dendogram(data = data, source = "rgbif")

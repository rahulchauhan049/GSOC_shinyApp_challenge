library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library("highcharter")
library("bdvis")
library("rgbif")
library("sqldf")
library("plotrix")

# key <- name_backbone(name = "Mammalia")$usageKey
# mammals <-occ_search(taxonKey = key, limit = 10000, hasCoordinate=TRUE, hasGeospatialIssue=FALSE, return = "data")
mammals <- read.csv("www/csv/mammalsLarge.csv")
mammals[which(names(mammals) %in% "rights") *-1]


write.csv(mammals,"www/csv/mammalsLarge.csv")
mammals <- read.csv("www/csv/hyenaData.csv")
mammals <- format_bdvis(mammals,source='rgbif')


names(mammals)=gsub("\\.","_",names(mammals))
if("Date_collected" %in% colnames(mammals)){
  if(length(which(!is.na(mammals$Date_collected)))==0){
    stop("Date_collected has no data")
  }
  dayofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%d"))
  weekofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%U"))
  monthofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%m"))
  Year_ = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%Y"))
  
} else {
  stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
}
a <- cbind(mammals["genus"],dayofYear,weekofYear,monthofYear,Year_)
a <- a %>% filter(genus %in% "Crocuta")
a<-arrange(a,as.numeric(a$dayofYear))
a<- a[c("genus", "dayofYear")]
a <- data.frame(table(a)) %>%rename(group = genus,
                                variable = dayofYear,
                                value = Freq)

plot_ly(a, x = ~variable, y = ~value, type = 'bar', color = ~group) %>%
  layout(title = "Features",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

df <- data.frame(x = c(1,2,3,4,5), y1 = c(5,5,5,5,5))

p <- plot_ly(df, x = ~x, y = ~y1, mode = 'markers')

p <- p %>% layout(
  title = "Button Restyle",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(
    list(
      type = "buttons",
      y = 0.8,
      buttons = list(
        
        list(method = "restyle",
             args = list("marker.symbol", "circle"),
             label = "Circle"),
        
        list(method = "restyle",
             args = list("marker.symbol", "square"),
             label = "Square")))
  ))
p

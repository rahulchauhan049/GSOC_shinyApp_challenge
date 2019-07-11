library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library("highcharter")
library("bdvis")
library("rgbif")
library("sqldf")
library("plotrix")

key <- name_backbone(name = "Mammalia")$usageKey
mammals <-occ_search(taxonKey = key, limit = 10000, hasCoordinate=TRUE, hasGeospatialIssue=FALSE, return = "data")
mammals <- read.csv("www/csv/mammalsLarge.csv")
mammals <- mammals[1:1000,]
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
a = cbind(mammals["genus"],dayofYear,weekofYear,monthofYear,Year_)
a<-arrange(a,as.numeric(a$Year_))
a<- a[c("genus", "Year_")]
a <- data.frame(table(a)) %>%rename(group = genus,
                                variable = Year_,
                                value = Freq)


plot_ly(a, x = ~variable, y = ~value, z = ~group, type = 'scatter3d', mode = 'lines', color = ~group)

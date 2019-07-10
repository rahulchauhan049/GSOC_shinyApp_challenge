library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library("highcharter")
library("bdvis")
library("rgbif")
library("sqldf")
library("plotrix")

mammals <- read.csv("www/csv/mammals.csv")
mammals <- format_bdvis(mammals,source='rgbif')


names(mammals)=gsub("\\.","_",names(mammals))
if("Date_collected" %in% colnames(mammals)){
  if(length(which(!is.na(mammals$Date_collected)))==0){
    stop("Date_collected has no data")
  }
  dayofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%j"))
  weekofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%U"))
  monthofYear = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%m"))
  Year_ = as.numeric(strftime(as.Date(mammals$Date_collected,na.rm=T), format = "%Y"))
  
} else {
  stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
}
a = cbind(mammals["family"],dayofYear,weekofYear,monthofYear,Year_)
a<-arrange(a,as.numeric(a$dayofYear))
a<- a[c("family", "dayofYear")]


weektab=sqldf("select dayofYear, count(*) as wct from a group by dayofYear")


ggplot(data=weektab,aes(x=dayofYear,y=wct))+
  geom_bar(stat="identity")+
  coord_polar()+
  scale_fill_brewer(palette="Greens")+xlab("")+ylab("")


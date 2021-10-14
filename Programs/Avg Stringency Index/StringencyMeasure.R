rm(list=ls())

setwd("/users/hansmarshall/Desktop/ASP/Group Assignment/Data")

library(lubridate)
library(plyr)
library(tidyverse)

#-------------------------------------------------------------------------------

measure <- read.csv("Measure.csv")

measure <- subset(measure, select = c("RegionName", "RegionCode", "Date", "StringencyIndex"))

#-------------------------------------------------------------------------------

typeof(measure$Date)
#integer

measure$date_num <- ymd(measure$Date)

typeof(measure$date_num)

measure$Date = NULL

measure$week <- strftime(measure$date_num, format="%V") #the other functions return the wrong week of year 

#-------------------------------------------------------------------------------

measure<-measure[!(measure$RegionName=="Rhode Island"),]

measure<-measure[!(measure$RegionName=="Washington DC"),]


measure<-measure[!(measure$RegionName==""),]

measure<-measure[!(measure$week>=30),]

measure <- measure[complete.cases(measure),]


#-------------------------------------------------------------------------------


length((which(is.na(measure$StringencyIndex))))

#-------------------------------------------------------------------------------

measure<- measure[with(measure, order(RegionName, date_num)), ] #ordering it again


avgIndex <- ddply(measure, c("RegionName", "week"), summarise, mean=mean(StringencyIndex))


test <- avgIndex %>%      #to see how many observations per group we have 
  group_by(RegionName) %>%
  count(RegionName)

unique(avgIndex$RegionName)


write.csv(avgIndex,"/users/hansmarshall/Desktop/ASP/Group Assignment/Data/avgIndex.csv", row.names = FALSE)


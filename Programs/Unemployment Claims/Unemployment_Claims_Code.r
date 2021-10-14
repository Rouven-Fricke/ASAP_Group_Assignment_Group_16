#######################
#Author: Rouven Fricke#
#######################

library(tidyverse)

library("readxl")
df <- read_csv("C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\r539cy.csv")
head(df)

df <- df[-1: -5,]
head(df)

df$`Refelcting Week Ended` <- gsub('/', '.', df$`Refelcting Week Ended`)

df <- df[ , !names(df) %in% c("Filed week ended")]
head(df)

#rearrange date
dateSubDay <- substring(df$`Refelcting Week Ended`,1,2)
dateSubMonth <- substring(df$`Refelcting Week Ended`,4,5)
dateSubYear <- substring(df$`Refelcting Week Ended`, 7,12)
dateSubYear
df$Date <- paste(dateSubYear, '-', dateSubDay, '-', dateSubMonth, sep="")                 

#Convert to date 
df$Date <- as.Date(df$Date, format =  "%Y-%m-%d")

#Remove dates that are still in 2019
df <- df[(!grepl('2019',df$Date)),]

#Add column week of year
df$WeekOfYear <- strftime(df$Date, format = "%V")

#rename cols
colnames(df)
dfFiltered <- df[, c("...1", "State", "Initial Claims", "Continued Claims", "Date", "WeekOfYear")]
names(dfFiltered)[names(dfFiltered) == '...1'] <- 'Counter'

head(dfFiltered)

#write to csv
write.csv(x=dfFiltered, file="C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Unemployment_Claims.csv")

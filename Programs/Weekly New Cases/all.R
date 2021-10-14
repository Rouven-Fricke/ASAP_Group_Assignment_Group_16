rm(list=ls())

setwd("~/Desktop/ASP/Group Assignment/")

dir <- "~/Desktop/ASP/Group Assignment/"

dirData <- paste0(dir, "Data/")

dirProg <- paste0(dir, "Programs/")

CD <- read.csv(file=paste0(dirData, "United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"))

library(data.table)
library(plyr)

#-------------------------------------------------------------------------------
#Ordering the observations by state and date
#-------------------------------------------------------------------------------

CD<- CD[with(CD, order(state, submission_date)), ]

#-------------------------------------------------------------------------------
#Cleaning the dataset, dropping variables
#-------------------------------------------------------------------------------

CD$conf_cases=NULL
CD$prob_cases=NULL
CD$pnew_death=NULL
CD$created_at=NULL
CD$consent_cases=NULL
CD$consent_deaths=NULL
CD$pnew_case=NULL
CD$prob_death=NULL
CD$conf_death=NULL


#-------------------------------------------------------------------------------
#Creating week of year indicator from dates
#-------------------------------------------------------------------------------

typeof(CD$submission_date)
#is still a character

CD$date_num <- as.Date(CD$submission_date, format = "%m/%d/%Y") #beware of the capital Y for 4 digit year!!

typeof(CD$date_num)
class(CD$date_num)


CD$week <- strftime(CD$date_num, format="%V") #the other functions return the wrong week of year 

CD$submission_date=NULL

CD$year <- year(CD$date_num)


#-------------------------------------------------------------------------------
#Dropping all observations from 2021
#-------------------------------------------------------------------------------


CD <- subset(CD, CD$year==2020 )

#we have no missing values in the dataset

#-------------------------------------------------------------------------------
#Calculate the average deaths and cases per week and state
#-------------------------------------------------------------------------------

CD<- CD[with(CD, order(state, date_num)), ] #ordering it again


tot_newcases <- ddply(CD, c("state", "week"), summarise, sum=sum(new_case))
tot_newdeaths <-ddply(CD, c("state", "week"), summarise, sum=sum(new_death))


avgNCases<- ddply(CD, c("state", "week"), summarize, mean=mean(new_case))
avgNDeaths <- ddply(CD, c("state", "week"), summarise, mean=mean(new_death))


avgCases <- ddply(CD, c("state", "week"),summarise,mean=mean(tot_death))
avgDeaths<-ddply(CD,c("state","week"),summarise,mean=mean(tot_cases))








#-------------------------------------------------------------------------------
#Merge the two dataframes avgCases and avgDeaths
#-------------------------------------------------------------------------------

#Ncases_Ndeaths <- merge(avgNCases, avgNDeaths, by=c("state", "week"))

Ncases_Ndeaths <- merge(tot_newcases, tot_newdeaths, by=c("state", "week"))

colnames(Ncases_Ndeaths) <- c("Code", "Week", "TotNCases", "TotNDeaths")



#-------------------------------------------------------------------------------
#Merge state codes and names
#-------------------------------------------------------------------------------


names <- read.csv(file=paste0(dirData, "Names1.csv"))

colnames(names) <- c("State", "Abbrev", "Code")


NCases_NDeaths <- merge(Ncases_Ndeaths, names, by="Code")

NCases_NDeaths[,"Abbrev"] =NULL

#NCases_NDeaths <- NCases_NDeaths[with(NCases_NDeaths, order(Code, Week)), ]

write.csv(NCases_NDeaths,"/users/hansmarshall/Desktop/ASP/Group Assignment/Data//NCases_NDeaths1.csv", row.names = FALSE)



#-------------------------------------------------------------------------------
#Include dummy variables for seasonality
#-------------------------------------------------------------------------------

rm(Ncases_Ndeaths, names, CD, avgDeaths, avgCases)























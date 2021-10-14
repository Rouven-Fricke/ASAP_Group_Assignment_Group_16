# Packages
library(tidyverse)
library(anytime)
library("readxl")
library(data.table)
library(plyr)
library(ggplot2)
library (plm)
library(lubridate)


#######################
#Author: Tobias Mayer##
#Topic: GSV_Data#######
#######################

# Define path
Path <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\GSV\\"

# Load the list of states
US_States <- read.csv(file = paste0(Path,"State_names.csv"), header = FALSE)[,1]
US_States[1] <- substring(US_States[1], 4,)
US_States_csv <- paste0(US_States, ".csv")

# Loop over all queries and all states to create dataframe
for (variable in c("anxiety", "depression", "insomnia", "suicide", "therapy", "workout")) {
  # Create new path for each query
  Path_change <- paste0(paste0(Path, variable),"\\")
  # Loop over all states per query
  for (i in 1:length(US_States)) {
    x <- read.csv(file = paste0(Path_change,US_States_csv[i]), skip = 1)
    colnames(x) <- c("Date", US_States[i])
    # Define data set if first state. else merge
    if (i == 1) {
      Df <- x
    } else {
      Df <- merge(Df, x, by="Date")
    } 
  }
  # Define dates
  Date <- format(as.Date(Df$Date), "%Y-%m-%d")
  # Create stacked dataframe
  Df_variables <- subset(Df, select = -Date )
  Df_final <- stack(Df_variables)
  # Reincorporate the dates
  Df_final$Date <- rep(Date, times = ncol(Df_variables))
  colnames(Df_final) <- cbind(variable, "State", "Date")
  # Define final dataframe
  if (variable == "anxiety") {
    DfAnxiety <- Df_final[, cbind("Date", "State", variable)]
  } else if (variable == "depression") {
    DfDepression <- Df_final[, cbind("Date", "State", variable)]
  } else if (variable == "insomnia") {
    DfInsomnia <- Df_final[, cbind("Date", "State", variable)]
  } else if (variable == "suicide") {
    DfSuicide <- Df_final[, cbind("Date", "State", variable)]
  } else if (variable == "therapy") {
    DfTherapy <- Df_final[, cbind("Date", "State", variable)]
  } else {
    DfWorkout <- Df_final[, cbind("Date", "State", variable)]
  }
  # Remove all temporary data
  rm(Df, Df_final, x, Date, i, Path_change, variable, Df_variables)
}


# Create the weighted index
DfIndex <- merge(DfAnxiety, 
                 merge(DfDepression,
                       merge(DfInsomnia,
                             merge(DfSuicide,DfTherapy, by=c("Date", "State")),
                             by=c("Date", "State")),
                       by=c("Date", "State")),
                 by=c("Date", "State"))

DfIndex <- 
  DfIndex %>%
  mutate(index = select(., c("anxiety", "depression", "insomnia", "suicide", "therapy")) %>% rowSums(na.rm = TRUE)/length(c("anxiety", "depression", "insomnia", "suicide", "therapy")))

DfIndex$WeekOfYear <- strftime(DfIndex$Date, format = "%V")
DfWorkout$WeekOfYear <- strftime(DfWorkout$Date, format = "%V")

rm(DfAnxiety, DfDepression, DfInsomnia, DfSuicide, DfTherapy, US_States_csv, US_States)

#######################
#Author: Tobias Mayer##
#Topic: SP_Data########
#######################

# Define path
Path <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\SP\\"

# Load Dataset
Dfsp <- read.csv(paste0(Path, "SP_Weekly_Prices.csv"), stringsAsFactors = FALSE)[,c(1,5)]

# Rename the columns and adjust the date
colnames(Dfsp) <- c("Date", "sp")

Dfsp$Date <- format(as.Date(anydate(Dfsp$Date)), "%Y-%m-%d")
Dfsp <- Dfsp[order(Dfsp$Date),]
Dfsp$sp_L1 <- dplyr::lag(Dfsp$sp,1)
Dfsp$sp <- (Dfsp$sp - Dfsp$sp_L1)/Dfsp$sp_L1
Dfsp$sp_L1 <- NULL
Dfsp$WeekOfYear <- strftime(Dfsp$Date, format = "%V")
Dfsp <- Dfsp[2:nrow(Dfsp),]

rm(Path)

#######################
#Author: Rouven Fricke#
#Topic: Unemployment###
#######################

df <- read_csv("C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Unemployment Claims\\r539cy.csv")
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
#write.csv(x=dfFiltered, file="C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Unemployment_Claims.csv")

rm(df)

rm(dateSubDay, dateSubMonth, dateSubYear)

#######################
#Author: Rouven Fricke#
#Topic: GCMR###########
#######################

#Load and inspect dataset
dfUS <- read_csv("C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\GCMR\\Global_Mobility_Report.csv")

#Filter that so that it only includes the US
dfUS <- subset(dfUS, country_region == "United States")
head(dfUS)

#Filter columns
dfUSFiltered <- dfUS[, c('country_region', 'sub_region_1', 'sub_region_2', 'census_fips_code', 'date', 
                         'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline',
                         'parks_percent_change_from_baseline', 'transit_stations_percent_change_from_baseline', 
                         'workplaces_percent_change_from_baseline', 'residential_percent_change_from_baseline')]

#Rename columns
#names(dfUSFiltered)[names(dfUSFiltered) == 'sub_region_1'] <- 'State'
dfUSFiltered <- dfUSFiltered %>%
  dplyr::rename(County = sub_region_2,
                State = sub_region_1)

#Filter out the county level
dfUSFiltered <- dfUSFiltered %>%
  subset(is.na(County))

#Filter out the state level
dfUSFiltered <- dfUSFiltered %>%
  subset(!is.na(State))

#Rearrange date
dateSubDay <- substring(dfUSFiltered$date,9,10)
dateSubMonth <- substring(dfUSFiltered$date,6,7)
dateSubYear <- substring(dfUSFiltered$date, 1,4)
dateSubYear
#dfUSFiltered$date <- paste(dateSubDay, '.', dateSubMonth, '.', dateSubYear, sep="")   
dfUSFiltered$date <- paste(dateSubYear, '-', dateSubDay, '-', dateSubMonth, sep="") 
head(dfUSFiltered$date)

#Convert character to date
dfUSFiltered$date <- as.Date(dfUSFiltered$date, format =  "%Y-%d-%m")

#Remove dates that are still in 2019 or in 2021 already
dfUSFiltered <- dfUSFiltered[(!grepl('2019',dfUSFiltered$date)),]
dfUSFiltered <- dfUSFiltered[(!grepl('2021',dfUSFiltered$date)),]

#Add column week of year
dfUSFiltered$WeekOfYear <- strftime(dfUSFiltered$date, format = "%V")

head(dfUSFiltered$WeekOfYear)

avgRecreation <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(retail_and_recreation_percent_change_from_baseline))
avgGrocery <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(grocery_and_pharmacy_percent_change_from_baseline)) 
avgParks <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(parks_percent_change_from_baseline))
avgTransit <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(transit_stations_percent_change_from_baseline))
avgWorkplace <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(workplaces_percent_change_from_baseline))
avgResidential <- ddply(dfUSFiltered, c("State", "WeekOfYear"), summarise, mean = mean(residential_percent_change_from_baseline))

#Merge columns and adjust names
dfMobilityMerge <- merge(avgRecreation, avgGrocery, by=c("State", "WeekOfYear"))
dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgRecreation = mean.x,
                AvgGrocery = mean.y)

dfMobilityMerge <- merge(dfMobilityMerge, avgTransit, by=c("State", "WeekOfYear"))
dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgTransit = mean)

dfMobilityMerge <- merge(dfMobilityMerge, avgParks, by=c("State", "WeekOfYear"))
dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgParks = mean)


#dfMobilityMerge <- dfMobilityMerge[ , -which(names(dfMobilityMerge) %in% c("mean.y"))]

dfMobilityMerge <- merge(dfMobilityMerge, avgWorkplace, by=c("State", "WeekOfYear"))

dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgWorkplace = mean)

dfMobilityMerge <- merge(dfMobilityMerge, avgResidential, by=c("State", "WeekOfYear"))

dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgResidential = mean)

dfMobilityMerge <- 
  dfMobilityMerge %>%
  mutate(Mobility_index = select(., c("AvgRecreation", "AvgWorkplace", "AvgResidential", "AvgGrocery", "AvgParks", "AvgTransit")) %>% 
           rowSums(na.rm = TRUE)/length(c("AvgRecreation", "AvgWorkplace", "AvgResidential", "AvgGrocery", "AvgParks", "AvgTransit")))

dfMobilityMerge <- 
  dfMobilityMerge %>%
  mutate(Mobility_index_excl.parks = select(., c("AvgRecreation", "AvgWorkplace", "AvgResidential", "AvgGrocery", "AvgTransit")) %>% 
           rowSums(na.rm = TRUE)/length(c("AvgRecreation", "AvgWorkplace", "AvgResidential", "AvgGrocery", "AvgTransit")))


rm(dfUS,dfUSFiltered, avgGrocery, avgParks, avgRecreation, avgResidential, avgTransit, avgWorkplace)
rm(dateSubDay, dateSubMonth, dateSubYear)

dfMobilityMerge

#write to a csv
#write.csv(x=dfMobilityMerge, file="C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Google_Mobility_Report_For_Regression.csv")

#######################
#Author: Hans Marshall#
#Topic: Covid/Death####
#######################

dir <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\State-level Weekly New Avg Deaths & Cases\\"
#dirData <- paste0(dir, "Data/")
#dirProg <- paste0(dir, "Programs/")
CD <- read.csv(file=paste0(dir, "United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"))

#Ordering the observations by state and date
CD<- CD[with(CD, order(state, submission_date)), ]

#Cleaning the dataset, dropping variables
CD$conf_cases=NULL
CD$prob_cases=NULL
CD$pnew_death=NULL
CD$created_at=NULL
CD$consent_cases=NULL
CD$consent_deaths=NULL
CD$pnew_case=NULL
CD$prob_death=NULL
CD$conf_death=NULL

#Creating week of year indicator from dates
typeof(CD$submission_date)
#is still a character

CD$date_num <- as.Date(CD$submission_date, format = "%m/%d/%Y") #beware of the capital Y for 4 digit year!!

typeof(CD$date_num)
class(CD$date_num)

CD$week <- strftime(CD$date_num, format="%V") #the other functions return the wrong week of year 

CD$submission_date=NULL

CD$year <- year(CD$date_num)
#Dropping all observations from 2021
CD <- subset(CD, CD$year==2020 )
#we have no missing values in the dataset

#Calculate the average deaths and cases per week and state
CD<- CD[with(CD, order(state, date_num)), ] #ordering it again

tot_newcases <- ddply(CD, c("state", "week"), summarise, sum=sum(new_case))
tot_newdeaths <-ddply(CD, c("state", "week"), summarise, sum=sum(new_death))

avgNCases<- ddply(CD, c("state", "week"), summarize, mean=mean(new_case))
avgNDeaths <- ddply(CD, c("state", "week"), summarise, mean=mean(new_death))

avgCases <- ddply(CD, c("state", "week"),summarise,mean=mean(tot_death))
avgDeaths<-ddply(CD,c("state","week"),summarise,mean=mean(tot_cases))

#Merge the two dataframes avgCases and avgDeaths
#Ncases_Ndeaths <- merge(avgNCases, avgNDeaths, by=c("state", "week"))

Ncases_Ndeaths <- merge(tot_newcases, tot_newdeaths, by=c("state", "week"))

colnames(Ncases_Ndeaths) <- c("Code", "Week", "TotNCases", "TotNDeaths")

#Merge state codes and names
names <- read.csv(file=paste0(dir, "Names1.csv"))

colnames(names) <- c("State", "Abbrev", "Code")


NCases_NDeaths <- merge(Ncases_Ndeaths, names, by="Code")

NCases_NDeaths[,"Abbrev"] =NULL

#NCases_NDeaths <- NCases_NDeaths[with(NCases_NDeaths, order(Code, Week)), ]

#write.csv(NCases_NDeaths,"/users/hansmarshall/Desktop/ASP/Group Assignment/Data//NCases_NDeaths1.csv", row.names = FALSE)


rm(dir, avgCases, avgDeaths, avgNCases, avgNDeaths, CD, names, Ncases_Ndeaths, tot_newcases, tot_newdeaths, avgIndex)


#######################
#Author: Tobias Mayer##
#Topic: DrugDeath######
#######################

dfDrug <- read_csv("C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Drug Overdose Death\\Monthly_drug.csv")

# Select columns
dfDrug <- dfDrug[, c("State", "Year", "Month", "Indicator", "Data Value", "State Name")]

# Filter data 
dfDrug <- dfDrug[(grepl('Number of Drug Overdose Deaths',dfDrug$Indicator)),]
dfDrug <- dfDrug[(grepl(2020,dfDrug$Year)),]

dfDrug <- dfDrug %>% dplyr::select(-State)
dfDrug <- dfDrug %>%
  dplyr::rename(State = `State Name`)
dfDrug <- dfDrug %>%
  dplyr::rename(Drug_death = `Data Value`)

months <- unique(dfDrug$Month)
months
#dfDrug$MonthNumber <- match(months, month.name)

# Crating Date
dfDrug <- dfDrug %>%
  mutate(MonthNumber = case_when(
    Month == 'January' ~ as.Date("2020-01-01", "%Y-%m-%d"),
    Month == 'February' ~ as.Date("2020-02-01", "%Y-%m-%d"),
    Month == 'March' ~ as.Date("2020-03-01", "%Y-%m-%d"),
    Month == 'April' ~ as.Date("2020-04-01", "%Y-%m-%d"),
    Month == 'May' ~ as.Date("2020-05-01", "%Y-%m-%d"),
    Month == 'June' ~ as.Date("2020-06-01", "%Y-%m-%d"),
    Month == 'July' ~ as.Date("2020-07-01", "%Y-%m-%d"),
    Month == 'August' ~ as.Date("2020-08-01", "%Y-%m-%d"),
    Month == 'September' ~ as.Date("2020-09-01", "%Y-%m-%d"),
    Month == 'October' ~ as.Date("2020-10-01", "%Y-%m-%d"),
    Month == 'November' ~ as.Date("2020-11-01", "%Y-%m-%d"),
    Month == 'December' ~ as.Date("2020-12-01", "%Y-%m-%d")
  ))

dfDrug$Month <- month(dfDrug$MonthNumber)

# Create help frame for upscaling
DfHelpDrug <- DfIndex[, c("Date", "State", "WeekOfYear")]
DfHelpDrug$Month <- month(DfHelpDrug$Date)

# Upscaing of the drug deaths
DfHelpDrug <- merge(DfHelpDrug, 
                    dfDrug[,c("Month","Drug_death","State")],
                    by = c("State", "Month"))

rm(months, dfDrug)

#######################
#Author: Hans Marshall#
#Topic: Stringency#####
#######################

measure <- read.csv("C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Measure.csv")

measure <- subset(measure, select = c("RegionName", "RegionCode", "Date", "StringencyIndex"))

typeof(measure$Date)
#integer

measure$date_num <- ymd(measure$Date)

typeof(measure$date_num)

measure$Date = NULL

measure$week <- strftime(measure$date_num, format="%V") #the other functions return the wrong week of year 

measure<-measure[!(measure$RegionName=="Rhode Island"),]

measure<-measure[!(measure$RegionName=="Washington DC"),]

measure<-measure[!(measure$RegionName==""),]

measure<-measure[!(measure$week>=30),]

measure <- measure[complete.cases(measure),]

length((which(is.na(measure$StringencyIndex))))

measure<- measure[with(measure, order(RegionName, date_num)), ] #ordering it again

avgIndex <- ddply(measure, c("RegionName", "week"), summarise, mean=mean(StringencyIndex))

test <- avgIndex %>%      #to see how many observations per group we have 
  group_by(RegionName) %>%
  count(RegionName)

unique(avgIndex$RegionName)

colnames(avgIndex) <- c("State", "WeekOfYear", "stringency")

rm(measure, test)

#######################
#Author: Tobias Mayer##
#Topic: Dataframe merge
#######################
#Note: only until week 52, last week (53) was not downloaded from google
#Note: only until week 51 in unemployment data


DfFinal <- merge(DfIndex %>% dplyr::select(-Date),
                 DfWorkout %>% dplyr::select(-Date),
                 by=c("WeekOfYear", "State"))

DfFinal <- merge(DfFinal,
                 Dfsp %>% dplyr::select(-Date),
                 by="WeekOfYear")

DfFinal <- merge(DfFinal,
                 NCases_NDeaths %>% dplyr::select(-Code),
                 by.x = c("WeekOfYear", "State"),
                 by.y = c("Week", "State"))

DfFinal <- merge(DfFinal,
                 dfFiltered[, c("State", "Initial Claims", "WeekOfYear")],
                 by = c("WeekOfYear", "State"))

DfFinal <- merge(DfFinal,
                 dfMobilityMerge,
                 by = c("WeekOfYear", "State"))

DfFinal <- merge(DfFinal,
                 DfHelpDrug[,c("State","WeekOfYear","Drug_death")],
                 by = c("WeekOfYear", "State"))

rm(DfIndex,DfWorkout,Dfsp,NCases_NDeaths,dfFiltered,dfMobilityMerge,DfHelpDrug)

# Rename Columns
colnames(DfFinal) <- c("WeekOfYear","State",
                       "GSV_anxiety","GSV_depression","GSV_insomnia","GSV_suicide","GSV_therapy","GSV_index","GSV_workout",
                       "SP","NCases","NDeaths","Unemplyment_claims","Recreation","Grocery","Transit","Parks","Workplace","Residential","Mobility_index","Mobility_index_excl_Parks",
                       "Drug_death"
                       )

# Drop States with NA's in the mobility data
ListDrop <- unique(DfFinal[(is.na(DfFinal$Transit)) | 
                      (is.na(DfFinal$Grocery)) |
                      (is.na(DfFinal$Recreation)) |
                      (is.na(DfFinal$Parks)) |
                      (is.na(DfFinal$Workplace)) |
                      (is.na(DfFinal$Residential)),]['State'])[[1]]

ListDrop <- c("Delaware", "Idaho", "North Dakota", "Wyoming", "Vermont", "Rhode Island", "Alaska", "South Dakota",  "New Hampshire",
                "Arkansas", "Iowa","Kansas" , "Maine", "Montana", "Nebraska", "West Virginia","Kentucky") 

DfFinal <- subset(DfFinal, !(DfFinal$State %in% ListDrop))

write.csv(DfFinal,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic.csv")

# Stringency index
DfStringency <- merge(DfFinal,
                      avgIndex,
                      by = c("WeekOfYear", "State"))

#######################
#Author: Tobias Mayer##
#Topic: Lagged Data###
#######################

IdentifierNames <- c("WeekOfYear","State")
ConstantNames <- c("GSV_anxiety","GSV_depression","GSV_insomnia","GSV_suicide","GSV_therapy","GSV_index")

### Normal data

# Only one Lag
DfFinalLag_1 <- DfFinal
DfFinalLag_1$WeekOfYear <- as.numeric(as.character(DfFinalLag_1$WeekOfYear)) + 1
colnames(DfFinalLag_1) <- c(IdentifierNames,ConstantNames,paste(colnames(DfFinalLag_1[,9:ncol(DfFinalLag_1)]), "L1", sep = "_"))
DfFinalLag_1$WeekOfYear <- ifelse(DfFinalLag_1$WeekOfYear == 8, "08", DfFinalLag_1$WeekOfYear)
DfFinalLag_1$WeekOfYear <- ifelse(DfFinalLag_1$WeekOfYear == 9, "09", DfFinalLag_1$WeekOfYear)

DfFinalLag_1 <- merge(DfFinal, 
                      DfFinalLag_1 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfFinalLag_1,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic_L1.csv")

# Up to two Lags
DfFinalLag_2 <- DfFinal
DfFinalLag_2$WeekOfYear <- as.numeric(as.character(DfFinalLag_2$WeekOfYear)) + 2
colnames(DfFinalLag_2) <- c(IdentifierNames,ConstantNames,paste(colnames(DfFinalLag_2[,9:ncol(DfFinalLag_2)]), "L2", sep = "_"))
DfFinalLag_2$WeekOfYear <- ifelse(DfFinalLag_2$WeekOfYear == 9, "09", DfFinalLag_2$WeekOfYear)

DfFinalLag_2 <- merge(DfFinalLag_1,
                      DfFinalLag_2 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfFinalLag_2,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic_L2.csv")

# Up to three Lags
DfFinalLag_3 <- DfFinal
DfFinalLag_3$WeekOfYear <- as.numeric(as.character(DfFinalLag_3$WeekOfYear)) + 3
colnames(DfFinalLag_3) <- c(IdentifierNames,ConstantNames,paste(colnames(DfFinalLag_3[,9:ncol(DfFinalLag_3)]), "L3", sep = "_"))

DfFinalLag_3 <- merge(DfFinalLag_2,
                      DfFinalLag_3 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfFinalLag_3,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic_L3.csv")

# Up to Four Lags
DfFinalLag_4 <- DfFinal
DfFinalLag_4$WeekOfYear <- as.numeric(as.character(DfFinalLag_4$WeekOfYear)) + 4
colnames(DfFinalLag_4) <- c(IdentifierNames,ConstantNames,paste(colnames(DfFinalLag_4[,9:ncol(DfFinalLag_4)]), "L4", sep = "_"))

DfFinalLag_4 <- merge(DfFinalLag_3,
                      DfFinalLag_4 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfFinalLag_4,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic_L4.csv")

# Up to five Lags
DfFinalLag_5 <- DfFinal
DfFinalLag_5$WeekOfYear <- as.numeric(as.character(DfFinalLag_5$WeekOfYear)) + 5
colnames(DfFinalLag_5) <- c(IdentifierNames,ConstantNames,paste(colnames(DfFinalLag_5[,9:ncol(DfFinalLag_5)]), "L5", sep = "_"))

DfFinalLag_5 <- merge(DfFinalLag_4,
                      DfFinalLag_5 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfFinalLag_5,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Basic_L5.csv")

### Stringency data

# Only one Lag
DfStringencyLag_1 <- DfStringency
DfStringencyLag_1$WeekOfYear <- as.numeric(as.character(DfStringencyLag_1$WeekOfYear)) + 1
colnames(DfStringencyLag_1) <- c(IdentifierNames,ConstantNames,paste(colnames(DfStringencyLag_1[,9:ncol(DfStringencyLag_1)]), "L1", sep = "_"))
DfStringencyLag_1$WeekOfYear <- ifelse(DfStringencyLag_1$WeekOfYear == 8, "08", DfStringencyLag_1$WeekOfYear)
DfStringencyLag_1$WeekOfYear <- ifelse(DfStringencyLag_1$WeekOfYear == 9, "09", DfStringencyLag_1$WeekOfYear)

DfStringencyLag_1 <- merge(DfStringency, 
                      DfStringencyLag_1 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfStringencyLag_1,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Stringency_L1.csv")

# Up to two Lags
DfStringencyLag_2 <- DfStringency
DfStringencyLag_2$WeekOfYear <- as.numeric(as.character(DfStringencyLag_2$WeekOfYear)) + 2
colnames(DfStringencyLag_2) <- c(IdentifierNames,ConstantNames,paste(colnames(DfStringencyLag_2[,9:ncol(DfStringencyLag_2)]), "L2", sep = "_"))
DfStringencyLag_2$WeekOfYear <- ifelse(DfStringencyLag_2$WeekOfYear == 9, "09", DfStringencyLag_2$WeekOfYear)

DfStringencyLag_2 <- merge(DfStringencyLag_1,
                           DfStringencyLag_2 %>% dplyr::select(-ConstantNames),
                      by=c("State", "WeekOfYear"))

write.csv(DfStringencyLag_2,"C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\Dataset_Stringency_L2.csv")

rm(ConstantNames, IdentifierNames, ListDrop)

#######################
#Author: Tobias Mayer##
#Topic: Summary Stat###
#######################

# Lag_2 Data
stargazer(DfFinalLag_2,
          summary = TRUE,
          median = TRUE,
          type="text")


# Stringency Lag_2 Data
stargazer(DfStringencyLag_2,
          summary = TRUE,
          median = TRUE,
          type="text")


#######################
#Author: Tobias Mayer##
#Topic: Basic Models###
#######################

library("Hmisc")

CorMatrix <- rcorr(as.matrix(DfFinalLag_2[, c("Recreation_L1","Grocery_L1","Transit_L1","Parks_L1","Workplace_L1","Residential_L1")]))


# Models
mdlBasic <- GSV_index ~ Recreation_L1 + Grocery_L1 + Transit_L1 + Parks_L1 + Workplace_L1
mdlBasicControll <- GSV_index ~ Recreation_L1 + Grocery_L1 + Transit_L1 + Parks_L1 + Workplace_L1 + NCases_L1 + NDeaths_L1 + Drug_death_L1 + GSV_workout_L1 + Unemplyment_claims_L1 + SP_L1

### Lag 1

# Pooled
Pooled_Basic <- lm(mdlBasic, data = DfFinalLag_2)
lmtest::bptest(Pooled_Basic)
# heteroscedasticity 
SE_Pooled_Basics <- sqrt(diag(vcovHC(Pooled_Basic , type ="HC0")))

Pooled_BasicControll <- lm(mdlBasicControll, data = DfFinalLag_2)
lmtest::bptest(Pooled_BasicControll)
# heteroscedasticity 
SE_Pooled_BasicControll <- sqrt(diag(vcovHC(Pooled_BasicControll , type ="HC0")))

stargazer(Pooled_Basic, Pooled_BasicControll, type="text")

# FE
FE_Basic <- plm(mdlBasic, data = DfFinalLag_2,
                index = c("State", "WeekOfYear"),
                model = "within")
lmtest::bptest(FE_Basic)
# heteroscedasticity 
SE_FE_Basic <- sqrt(diag(vcovHC(FE_Basic , type ="HC0")))

FE_BasicControll <- plm(mdlBasicControll, data = DfFinalLag_2,
                        index = c("State", "WeekOfYear"),
                        model = "within")
lmtest::bptest(FE_BasicControll)
# heteroscedasticity 
SE_FE_BasicControll <- sqrt(diag(vcovHC(FE_BasicControll , type ="HC0")))

stargazer(FE_Basic, FE_BasicControll,  type="text")

# RE
RE_Basic <- plm(mdlBasic, data = DfFinalLag_2,
                index = c("State", "WeekOfYear"),
                model = "random")
lmtest::bptest(RE_Basic)
# heteroscedasticity 
SE_RE_Basic <- sqrt(diag(vcovHC(RE_Basic , type ="HC0")))

RE_BasicControll <- plm(mdlBasicControll, data = DfFinalLag_2,
                        index = c("State", "WeekOfYear"),
                        model = "random")
lmtest::bptest(RE_BasicControll)
# heteroscedasticity 
SE_RE_BasicControll <- sqrt(diag(vcovHC(RE_BasicControll , type ="HC0")))

stargazer(RE_Basic, RE_BasicControll, type="text")

### Lag 2

# FE
mdlBasicControll_L2 <- GSV_index ~ Recreation_L2 + Grocery_L2 + Transit_L2 + Parks_L2 + Workplace_L2 + NCases_L2 + NDeaths_L2 + Drug_death_L2 + GSV_workout_L2 + Unemplyment_claims_L2 + SP_L2


FE_BasicControll_L2 <- plm(mdlBasicControll_L2, data = DfFinalLag_2,
                        index = c("State", "WeekOfYear"),
                        model = "within")
lmtest::bptest(FE_BasicControll_L2)
# heteroscedasticity 
SE_FE_BasicControll_L2 <- sqrt(diag(vcovHC(FE_BasicControll_L2 , type ="HC0")))

stargazer(FE_BasicControll_L2,  type="text")


# Tests

#### Normality
P1 <- qplot(sample = DfFinalLag_2$GSV_index, stat="qq")
P2 <- qplot(sample = DfFinalLag_2$Recreation_L1, stat="qq")
P3 <- qplot(sample = DfFinalLag_2$Grocery_L1, stat="qq")
P4 <- qplot(sample = DfFinalLag_2$Transit_L1, stat="qq")
P5 <- qplot(sample = DfFinalLag_2$Parks_L1, stat="qq")
P6 <- qplot(sample = DfFinalLag_2$Workplace_L1, stat="qq")
P7 <- qplot(sample = DfFinalLag_2$Residential_L1, stat="qq")
P8 <- qplot(sample = DfFinalLag_2$NCases_L1, stat="qq")
P9 <- qplot(sample = DfFinalLag_2$NDeaths_L1, stat="qq")
P10 <- qplot(sample = DfFinalLag_2$Drug_death_L1, stat="qq")
P11 <- qplot(sample = DfFinalLag_2$GSV_workout_L1, stat="qq")
P12 <- qplot(sample = DfFinalLag_2$Unemplyment_claims_L1, stat="qq")
P13 <- qplot(sample = DfFinalLag_2$SP_L1, stat="qq")

require(gridExtra)
grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, 
             ncol = 4, nrow= 4)

#### Model specoific

# Pooled vs FE
pFtest(FE_Basic, Pooled_Basic)
# Pooled model is rejected
pFtest(FE_BasicControll, Pooled_BasicControll)
# Pooled model is rejected

# Random vs Fixed
phtest(FE_Basic, RE_Basic)
# reject H0, take FE
phtest(FE_BasicControll, RE_BasicControll)
# reject H0, take FE


# Multicollinearity

# Lag 1

vif(Pooled_Basic) 
vif(Pooled_BasicControll)
# slightly multicollinearity all below 10

vif(FE_Basic)
vif(FE_BasicControll)
# does not work

# Lag 2
vif(lm(mdlBasicControll_L2, DfFinalLag_2))
# slight multicollinearity with one at 10.5


# Summary of the results

stargazer(FE_Basic, FE_BasicControll,
          type = "text",
          add.lines = list(c("White's SE", "Yes", "Yes")),
          se = list(SE_FE_Basic,SE_FE_BasicControll),
          df = FALSE)

stargazer(FE_BasicControll_L2,
          type = "text",
          add.lines = list(c("White's SE", "Yes")),
          se = list(SE_FE_BasicControll_L2),
          df = FALSE)

#######################
#Author: Tobias Mayer##
#Topic: Fancy Models###
#######################

# Beginning of the year: until end of april - week 18
DfFinalLag_2.beg <- DfFinalLag_2[DfFinalLag_2$WeekOfYear < 19,]

FE_BasicControll.beg <- plm(mdlBasicControll, data = DfFinalLag_2.beg,
                        index = c("State", "WeekOfYear"),
                        model = "within")
lmtest::bptest(FE_BasicControll.beg)
# heteroscedasticity 
SE_FE_BasicControll.beg <- sqrt(diag(vcovHC(FE_BasicControll.beg , type ="HC0")))

vif(lm(mdlBasicControll,DfFinalLag_2.beg ))
# Multicollinearity

stargazer(FE_BasicControll.beg, type = "text")

# Stingency
DfStringrencyLag_2.beg <- DfStringencyLag_2[DfStringencyLag_2$WeekOfYear < 19,]

mdlBasicControllStringrency <- GSV_index ~ Recreation_L1 + Grocery_L1 + Transit_L1 + Parks_L1 + Workplace_L1 + NCases_L1 + NDeaths_L1 + Drug_death_L1 + GSV_workout_L1 + Unemplyment_claims_L1 + SP_L1 + stringency_L1

FE_BasicControllStringrency.beg <- plm(mdlBasicControllStringrency, data = DfStringrencyLag_2.beg,
                            index = c("State", "WeekOfYear"),
                            model = "within")
lmtest::bptest(FE_BasicControllStringrency.beg)
# heteroscedasticity 
SE_FE_BasicControllStringrency <- sqrt(diag(vcovHC(FE_BasicControllStringrency.beg , type ="HC0")))

vif(lm(mdlBasicControllStringrency,DfStringrencyLag_2.beg ))
# Multicollinearity

stargazer(FE_BasicControllStringrency.beg, type = "text")

# Rest of the year

DfFinalLag_2.end <- DfFinalLag_2[DfFinalLag_2$WeekOfYear > 18,]

FE_BasicControll.end <- plm(mdlBasicControll, data = DfFinalLag_2.end,
                            index = c("State", "WeekOfYear"),
                            model = "within")
lmtest::bptest(FE_BasicControll.end)
# heteroscedasticity 
SE_FE_BasicControll.end <- sqrt(diag(vcovHC(FE_BasicControll.end , type ="HC0")))

vif(lm(mdlBasicControll,DfFinalLag_2.end ))
# No proof for multicollinearity - highest value of 6.8

stargazer(FE_BasicControll.end, type = "text")

# Summary of the results

stargazer(FE_BasicControll.beg, FE_BasicControllStringrency.beg,FE_BasicControll.end,
          type = "text",
          add.lines = list(c("White's SE", "Yes", "Yes","Yes")),
          se = list(SE_FE_BasicControll.beg,SE_FE_BasicControllStringrency,SE_FE_BasicControll.end),
          df = FALSE)

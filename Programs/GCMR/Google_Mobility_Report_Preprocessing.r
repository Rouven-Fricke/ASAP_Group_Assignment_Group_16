#######################
#Author: Rouven Fricke#
#######################


library(tidyverse)

#Load and inspect dataset
dfUS <- read_csv("C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Global_Mobility_Report.csv")



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
  dplyr::rename(AvgTransit = mean.x)

dfMobilityMerge <- merge(dfMobilityMerge, avgParks, by=c("State", "WeekOfYear"))
dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgParks = mean.x)


dfMobilityMerge <- dfMobilityMerge[ , -which(names(dfMobilityMerge) %in% c("mean.y"))]

dfMobilityMerge <- merge(dfMobilityMerge, avgWorkplace, by=c("State", "WeekOfYear"))

dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgWorkplace = mean)

dfMobilityMerge <- merge(dfMobilityMerge, avgResidential, by=c("State", "WeekOfYear"))

dfMobilityMerge <- dfMobilityMerge %>%
  dplyr::rename(AvgResidential = mean)

rm(dfUS)
rm(dfUSFiltered)

dfMobilityMerge

#write to a csv
#write.csv(x=dfMobilityMerge, file="C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Google_Mobility_Report_For_Regression.csv")

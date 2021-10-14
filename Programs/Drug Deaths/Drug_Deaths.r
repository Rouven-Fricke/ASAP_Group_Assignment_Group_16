library(tidyverse)


dfDrug <- read_csv("C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Group Assignment\\Monthly_drug.csv")



#Select cols
dfDrug <- dfDrug[, c("State", "Year", "Month", "Indicator", "Data Value", "State Name")]
head(dfDrug)

#Filter 
dfDrug <- dfDrug[(grepl('Number of Deaths',dfDrug$Indicator)),]
dfDrug <- dfDrug[(grepl(2020,dfDrug$Year)),]
dfDrug

months <- unique(dfDrug$Month)
months
#dfDrug$MonthNumber <- match(months, month.name)

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

#Get first week of year for first week of month
dfDrug$WeekOfYear <- strftime(dfDrug$MonthNumber, format = "%V")
dfDrug$WeekOfYear <- as.numeric(dfDrug$WeekOfYear)

#dfDrugBackup <- dfDrug
dfDrug

#Loop over the list of weeks
#In a second loop, take week mod 4 three times and append the new weeks to the list 
weekList <- dfDrug$WeekOfYear
weekList
modList <- c()
for(i in weekList)
  for(j in 0:3)
    modList <- c(modList, i + j%%4)
modList




#duplicate each month four times.
dfDrug <-  dfDrug[rep(seq_len(nrow(dfDrug)), each = 4), ]
#dfDrugBackup <- dfDrugBackup[rep(seq_len(nrow(dfDrugBackup)), each = 4), ]
#dfDrugBackup$WeekOfYear <- as.numeric(dfDrugBackup$WeekOfYear)

#add the list of all the weeks to the df


dfDrug$CompleteWeeks <- modList 

dfDrug <- dfDrug[ , !names(dfDrug) %in% c("WeekOfYear")] 

dfDrug <- dfDrug %>%
  dplyr::rename(WeekOfYear = CompleteWeeks)


dfDrug <- dfDrug[ , -which(names(dfDrug) %in% c("MonthNumber"))] 
dfDrug
#######################
#Author: Tobias Mayer##
#######################

# Packages
library(tidyverse)


# Define path
Path <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\GSV\\"

# Load the list of states
US_States <- read.csv(file = paste0(Path,"State_names.csv"))[,1]
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
  Date <- format(as.Date(Df$Date), "%d.%m.%Y")
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
  rm(Df, Df_final, x, Date, i, Path_change, variable)
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

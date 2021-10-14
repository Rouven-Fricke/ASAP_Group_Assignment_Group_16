#######################
#Author: Tobias Mayer##
#######################

# Packages
library(anytime)

# Define path
Path <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\SP\\"

# Load Dataset
Dfsp <- read.csv(paste0(Path, "SP_Weekly_Prices.csv"), stringsAsFactors = FALSE)[,c("Date", "Close.")]

# Rename the columns and adjust the date
colnames(Dfsp) <- c("Date", "sp")
Dfsp$Date <- format(as.Date(anydate(Dfsp$Date)), "%d.%m.%Y")


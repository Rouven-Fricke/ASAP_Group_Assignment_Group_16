Path <- "C:\\Users\\rouma\\Documents\\Wirtschaftsinformatik\\Master\\Advanced Statistics and Programming\\Group Assignment\\"
file <- "Dataset_Basic_L2.csv"

DfFinalLag_2 <- read_csv(paste0(Path, file))

#Make some scatter plots. They show little correlation, do not add a lot of value,
#could possibly omit them.
# I do not think we need to add it to the report.

ggplot(data=DfFinalLag_2, aes(x = Residential_L2, y=GSV_index)) +
  geom_point(size = 0.5) + 
  geom_smooth(method="lm")


ggplot(data=DfFinalLag_2, aes(x = Transit_L2, GSV_index)) +
  geom_point(size = 0.5) + 
  geom_smooth(method="lm")

colnames(DfFinalLag_2)


ggplot(data=DfFinalLag_2, aes(x = Recreation_L2, GSV_index)) +
  geom_point(size = 0.5) + 
  geom_smooth(method="lm")

ggplot(data=DfFinalLag_2, aes(x = Grocery_L2, GSV_index)) +
  geom_point(size = 0.5) + 
  geom_smooth(method="lm")

length(DfFinalLag_2$Recreation_L2)

ggplot(data=DfFinalLag_2, aes(x = Workplace_L2, GSV_index)) +
  geom_point(size = 0.5) + 
  geom_smooth(method="lm")


colnames(DfFinalLag_2)

#############
##Create two new data framees for average and difference values
#############

dfL2Avg <- ddply(DfFinalLag_2, .(State), summarise,
                 avg.GSV_index = mean(GSV_index, na.rm = TRUE),
                 avg.GSV_workout_L1 = mean(GSV_workout_L1, na.rm = TRUE),
                 avg.SP_L1 = mean(SP_L1, na.rm = TRUE),
                 avg.NCases_L1 = mean(NCases_L1, na.rm = TRUE),
                 avg.NDeaths_L1 = mean(NDeaths_L1, na.rm = TRUE),
                 avg.Unemplyment_claims_L1 = mean(Unemplyment_claims_L1, na.rm = TRUE),
                 avg.Recreation_L1 = mean(Recreation_L1, na.rm = TRUE),
                 avg.Grocery_L1 = mean(Grocery_L1, na.rm = TRUE),
                 avg.Transit_L1 = mean(Transit_L1, na.rm = TRUE),
                 avg.Parks_L1 = mean(Parks_L1, na.rm = TRUE),
                 avg.Workplace_L1 = mean(Workplace_L1, na.rm = TRUE),
                 avg.Residential_L1 = mean(Residential_L1, na.rm = TRUE),
                 avg.Mobility_index_L1 = mean(Mobility_index_L1, na.rm = TRUE),
                 avg.Mobility_index_excl_Mobility_L1 = mean(Mobility_index_excl_Mobility_L1, na.rm = TRUE),
                 avg.Drug_death_L1 = mean(Drug_death_L1, na.rm = TRUE))

dfL2.sub <- merge(DfFinalLag_2, dfL2Avg, by="State")

attach(dfL2.sub)
dfL2.sub$diff.GSV_index <- GSV_index - avg.GSV_index
dfL2.sub$diff.GSV_workout_L1   <- GSV_workout_L1   - avg.GSV_workout_L1
dfL2.sub$diff.SP_L1 <- SP_L1 - avg.SP_L1
dfL2.sub$diff.NCases_L1 <- NCases_L1 - avg.NCases_L1
dfL2.sub$diff.Unemplyment_claims_L1 <- Unemplyment_claims_L1 - avg.Unemplyment_claims_L1
dfL2.sub$diff.Recreation_L1 <- Recreation_L1 - avg.Recreation_L1
dfL2.sub$diff.Grocery_L1 <- Grocery_L1 - avg.Grocery_L1
dfL2.sub$diff.Transit_L1 <- Transit_L1 - avg.Transit_L1
dfL2.sub$diff.Parks_L1 <- Parks_L1 - avg.Parks_L1
dfL2.sub$diff.Workplace_L1 <- Workplace_L1 - avg.Workplace_L1
dfL2.sub$diff.Residential_L1 <- Residential_L1 - avg.Residential_L1
dfL2.sub$diff.Mobility_index_L1 <- Mobility_index_L1 - avg.Mobility_index_L1
dfL2.sub$diff.Mobility_index_excl_Mobility_L1 <- Mobility_index_excl_Mobility_L1 - avg.Mobility_index_excl_Mobility_L1
dfL2.sub$diff.Drug_death_L1 <- Drug_death_L1 - avg.Drug_death_L1
detach(dfL2.sub)

#Between variation
#Possibly omit aes(colour=State) in geom_point method as there are too many states
#Also possible to do a subset of states to do it more legible, could discuss that

#Subset the data set to only include selected states. This makes the within variation plots
#more legible, less crowded.
dfL2.sub_SelectedStates <- dfL2.sub[dfL2.sub$State %in% c("California", "Colorado", "Florida", "Illionois", "Oklahoma"),]



ggplot(dfL2Avg, aes(x = avg.Workplace_L1, y = avg.GSV_index)) + 
  geom_point(aes(colour=State), size = 3) + 
  geom_smooth(method="lm", se=FALSE, colour="black") +
  xlab("Workplace Mobility Changes") + 
  ylab("GSV Index")



#Within variation
ggplot(dfL2.sub_SelectedStates, aes(x = diff.Grocery_L1,  y = diff.GSV_index)) +
  geom_point(aes(colour=State), size = 3) +
  geom_smooth(method="lm", se=FALSE, colour="black") +
  annotate("point", x=0, y=0, size=3) +
  xlab("Within variation: Workplace Mobility") +
  ylab("GSV Index")
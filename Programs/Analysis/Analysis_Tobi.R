#######################
#Author: Tobias Mayer##
#Topic: Basic Models###
#######################
library(tidyverse)
library(anytime)
library("readxl")
library(data.table)
library(plyr)
library(ggplot2)
library (plm)
library(lubridate)
library("Hmisc")


Path <- "C:\\Users\\Tobias Mayer\\Desktop\\RSM\\Advanced Statistics & Programming\\Group_Assignment\\"

DfFinalLag_2 <-  read.csv(file = paste0(Path, "Dataset_Basic_L2.csv"))
DfStringencyLag_2 <- read.csv(file = paste0(Path, "Dataset_Stringency_L2.csv"))


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
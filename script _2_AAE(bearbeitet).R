#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Downloads")



#install packages and load the library 
install.packages("readxl")
library(readxl)


#read xlsx data
ADV_SALES_DATA <- read_xlsx("/Users/aylatan/Downloads/ADV_SALES_DATA.xlsx")


# Convert to a data frame, if necessary
ADV_SALES_DATA <- as.data.frame(ADV_SALES_DATA)

# Now you can check the structure to confirm
str(ADV_SALES_DATA)


# Correctly format the formula using the column names
lm_result <- lm(formula = Sales ~ Adv, data = ADV_SALES_DATA)


# Regression analysis
 #advertising-sales example from the slides

lm(formula = ADV_SALES_DATA$Sales~ADV_SALES_DATA$Adv, data=ADV_SALES_DATA)
lm(formula = Sales~Adv, data=ADV_SALES_DATA) #yields same result
OLS <- lm(formula = Sales~Adv, data=ADV_SALES_DATA) # to store the result
print(OLS)
summary(OLS)


#include squared advertising (first generate squared adv variable)
ADV_SALES_DATA$Adv2 <- ADV_SALES_DATA$Adv*ADV_SALES_DATA$Adv 
OLS2 <- lm(formula = Sales~ Adv +Adv2, data=ADV_SALES_DATA)
summary(OLS2)

#firm profit data set
OLS_firm_profit <- lm(formula = Profit ~ Firmsize +  Age, data = firm_profit) #standard regression analysis
summary(OLS_firm_profit)

#include squared firm size
firm_profit$size2 <- firm_profit$Firmsize*firm_profit$Firmsize
OLS_firm_profit2 <- lm(formula = Profit ~ Firmsize + +size2 + Age, data = firm_profit) 
summary(OLS_firm_profit2)
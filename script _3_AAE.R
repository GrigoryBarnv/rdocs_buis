# Regression analysis with dummy variables
  # relevance of firm and industry

rm(list=ls())

library(readxl)

setwd("/Users/aylatan/Downloads")

food_prof_germ <- read_excel("MarketPower.xlsx")

install.packages('fastDummies')
library('fastDummies')

food_prof_germ <- dummy_cols(food_prof_germ, select_columns = 'NACE_industry')

industry <- lm(formula = PROF ~ NACE_industry_1511 +NACE_industry_1512 +NACE_industry_1513 +NACE_industry_1520+NACE_industry_1531 +NACE_industry_1533+NACE_industry_1541 +NACE_industry_1551+NACE_industry_1552 +NACE_industry_1561+NACE_industry_1562 +NACE_industry_1571+NACE_industry_1572 +NACE_industry_1581+NACE_industry_1582 +NACE_industry_1583+NACE_industry_1584 +NACE_industry_1586 +NACE_industry_1587+NACE_industry_1591 +NACE_industry_1593+NACE_industry_1596+NACE_industry_1597 +NACE_industry_1598, data = food_prof_germ) 
summary(industry)

industry1 <- lm(formula = PROF ~ factor(NACE_industry), data = food_prof_germ) 
summary(industry1)

firm <- lm(formula = PROF ~ factor(index), data = food_prof_germ) 
summary(firm)


# profit persistence 
profit_persist <- lm(formula = PROF ~ lag_PROF, data = food_prof_germ) 
summary(profit_persist)

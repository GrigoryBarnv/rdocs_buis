#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Desktop/rdocs for modul econmic argiculture")

#read csv data
data = read.csv2("CobbDouglasData.csv")


attach(data)
names(data)


options(scipen=16) # this command allows more digits to be printed in the

reg2 = lm(output ~ capital)

summary(reg2)

theme_set(theme_classic()) # this is the style of the diagram
model.diag.metrics <- augment(reg2) # this command writes residuals and
ggplot(model.diag.metrics, aes(capital, output)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = capital, yend = .fitted), color = "red", size = 0.3)



1 - (var(residuals(reg2))/var(output))

m1 = lm(log(output) ~ log(capital) + log(labor))

summary(m1)

b1 = coefficients(m1)[2]

b2 = coefficients(m1)[3]

b1+b2

install.packages("car")
library(car)

linearHypothesis(m1, "log(labor)+log(capital)=1",test="F")


install.packages("systemfit")
library("systemfit") #invoke the needed package of R

model = log(output) ~ log(capital) + log(labor)
restriction = c("eq1_log(capital) + eq1_log(labor) = 1")
m2 = systemfit(model, restrict.matrix=restriction, method = "OLS")

b1 = coefficients(m2)[2]
b2 = coefficients(m2)[3]
b1+b2

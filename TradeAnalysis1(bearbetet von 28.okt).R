#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Desktop/rdocs for modul econmic argiculture")

#read csv data
data = read.csv2("CobbDouglasData.csv")

install.packages("readxl")

#download the package before every session
library(readxl)

#The `attach()` function in R is used to make data frame variables or lists directly accessible by their names without explicitly referencing the data frame or list.

attach(data)
names(data)


#analize the data 
summary(data)

#easy plot 
plot(output, year)


#make an easy histogramm
hist(output)
hist(capital)

plot(output, year, col="dark red")

plot(log(output) ~ log(capital), col="dark red")


##The standard Ordinary Least Squares (OLS) model is estimated as follows:
##basic linear modell

linear_model_1=lm(log(output) ~ log(capital))

linear_model_1

a <- cov(log(capital),log(output))
b <- var(log(capital))
a/b

#summary directly after istemation linear or regression modell
summary(linear_model_1)


#calculate t 
beta_capital <- 0.553959/0.040491
beta_capital

#intercept only model  1 shows us we only take intercept in account
m0<-lm(log(output)~ 1)
summary(m0)

mean(log(output))

#auto
install.packages("lattice")
library(lattice)

#make a xyplot of the data
xyplot(predict(m0) +log(output)~year, auto.key = TRUE)


#
#
#.   missed code piece 
#
#
WWI # verify that the dummy is 1 in the War-years

## [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0

m1 <- lm(log(output)~0+WWI) # remove the intercept

summary(m1)
#



#plot some dummies variables
xyplot(predict(ml)+log(output)~year, auto.key = TRUE)

mean(log(output)[WWI==1])

mean(log(output)[WWI==0])

m2 <-lm(log(output)~WWI)
summary(m2)

xyplot(predict(m2)+log(output)~year, auto.key = TRUE)

m3<-lm(log(output)~log(capital)+WWI)
summary(m3)

xyplot(predict(m3)+log(output)~year,auto.key = TRUE)
  
#is m 4 modell
m4<-lm(log(output)~log(capital)+log(labor)+WWI)
summary(m4)
#plot the modell 4 
xyplot(predict(m4)+log(output)~year,auto.key = TRUE)

#including the interaction of labor input was more productive
#in world war time
m5<-lm(log(output)~log(capital)+log(labor)+I(log(labor)*WWI)+WWI)
summary(m5)


######


#we want to create a dummy variable 
WWIusa<-rep(0,nrow(data))

#in the year 1917 we want to add 1 to the dataset
#instead of the 0 
WWIusa[year==1917]<-1
WWIusa[year==1918]<-1
cbind(year,WWIusa)

#we want to put data in our dataset
data$WWIusa<-rep(0,nrow(data))
data$WWIusa[year==1917]<-1
data$WWIusa[year==1918]<-1

options(scipen=16) # this command allows more digits to be printed in the
reg2 = lm(output ~ capital)
summary(reg2)|
  
m1 = lm(log(output) ~ log(capital) + log(labor))


library(ggplot2) # the following are libraries that we need for the graph


library(tidyverse)

library(broom)

theme_set(theme_classic()) # this is the style of the diagram

model.diag.metrics <- augment(reg2) # this command writes residuals and

#predicted values in one object, the next lines make the plot using the ggplot

#command
#104
ggplot(model.diag.metrics, aes(capital, output)) +
 # 105
geom_point() +
  #106
stat_smooth(method = lm, se = FALSE) +
  #107
geom_segment(aes(xend = capital, yend = .fitted), color = "red", size =
             0.3)
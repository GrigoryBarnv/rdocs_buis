#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Desktop/rdocs for modul econmic argiculture/")

#read csv data
data = read.csv2("/Users/aylatan/Desktop/rdocs for modul econmic argiculture/CobbDouglasData.csv")

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
xyplot(predict(m0)+log(output)~year, auto.key = TRUE)

WWI
#0 means we remove intercept information and we want to look only at ww1 dummie variable
#effect of ww1 equals 0 is rejected
ml<-(log(output)~0+WWI)
summary(ml)

xyplot(predict(ml)+log(output)~year, auto.key = TRUE)


##tutorial 2 #####################
####
####

summary(data)
attach(data)
options (scipen=16) ## to reduce the commas 

reg2 <- lm(output~capital, data= data)
summary(reg2)

#install packages 

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("broom")
library("ggplot2")
library("tidyverse")
library("broom")


theme_set(theme_classic())
model.diag.metrics<-augment(reg2, data = data)


######DOESNOT WORK
ggplot(model.diag.metrics, aes(capital, output))+
  geom_point()+
  stat_smooth(method=lm, se=FALSE)+
  geom_segment(aes(xend=capital,yend=.fitted),color="red", size=0,3)



######WORKS 
ggplot(model.diag.metrics, aes(x = capital, y = output)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = capital, yend = .fitted), 
               color = "red", 
               size = 0.3)

cbind(output,predict(reg2), residuals(reg2))

var(predict(reg2))/var(output)
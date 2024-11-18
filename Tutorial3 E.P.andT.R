#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Downloads")

#read csv data
data2 = read.csv2("FADN_grain_producers_2012.csv")

install.packages("readxl")

#download the package before every session
library(readxl)

#The `attach()` function in R is used to make data frame variables or lists directly accessible by their names without explicitly referencing the data frame or list.

attach(data2) # attach data to work with the same data inside one project, if there is only one dataframe 
names(data2)


#analize the data 
summary(data2)
table(Country)
table(Region)

Y <- data2$CropOutput

options(scipen=16)

hist(Y, main="Crop~Output for COP \n un avalible EU regions 2012 ")

plot(Hectares, CropOutput)
text(Hectares, CropOutput, Region)


#calculate the avarage product 
APland <-CropOutput/Hectares
APfert<- CropOutput/Xfert
APpest<-CropOutput/Xpest
APlab<- CropOutput/Xlabtotal
APseed<- CropOutput/Xseeds
APland

cbind(Region,APland) #bind region and APland in one dataframe ???
boxplot(APland)
boxplot(APland~Country)
#avarage product of a certain production factor is nor 
#reasanoble to evaluate the productyvity in certain region
#so we use The TFP - total factor productivity 

install.packages("lattice")
library(lattice)

xyplot(APland~Hectares | Country, type=c("p","r"),scale="free") # we ask for


stripParams <- list(cex=0.6, lines=2) #formatting of headlines
xyplot(APland ~ Hectares|Country,
       type=c("p","r"),scales="free",par.strip.text = stripParams)       



data2<-cbind(data2,APland)     
install.packages("dplyr")
library(dplyr)

data2 %>%
  group_by(Country) %>%  
  summarise_at(vars(APland),
          funs(mean(., na.rm=TRUE)))             


TFP <- CropOutput/(Hectares + Xseeds + Xfert + Xpest + Xother + Xlabtotal)             

min(TFP)

max(TFP)

mean(TFP)

median(TFP)

par(mfrow=c(2,2))
plot(TFP ~ APland, cex=0.5)
plot(TFP ~ APfert, cex=0.5)
plot(TFP ~ APpest, cex=0.5)
plot(TFP ~ APlab, cex=0.5) 

plot(Hectares, TFP, cex=0.3)
text(Hectares,TFP, Region, cex=0.3)
#the TFP liinear related to the increase of land - there is different clusters
# of the lines 


######we want to formulate a linear production function 
linear.profu<-lm(CropOutput~Hectares+Xfert+Xpest+Xlabtotal)
summary(linear.profu)

##1.an insignificant intercept does not make sence -> in our 
#production function we want ins.int
#2.not all insignificant coefficients make no sence and we have to leave them out 
#becouse without the labour input the whole vodel doesnt make aby sence
#3.Coeff. tells us - if we add 1 additional unit of fertilizer we have 263.7 increase in profit 


#explore what variables we have to leave ib the model and which no
#
install.packages("olsrr")
library("olsrr")
coef(linear.profu)

ols_step_forward_p(linear.profu)


coef(linear.profu)
e_land <- coef(linear.profu)[2]*Hectares/fitted(linear.profu)
e_fert <- coef(linear.profu)[3]*Xfert/fitted(linear.profu) 
e_pest <- coef(linear.profu)[4]*Xpest/fitted(linear.profu) 
e_lab <- coef(linear.profu)[5]*Xlabtotal/fitted(linear.profu)
mean(e_land)


par(mfrow=c(2,2))
hist(e_land)


#we calculate the factor elasticity to chek 
#the returns to scale 
se<-e_land+e_fert+e_pest+e_lab
se


cbind(min(se),max(se),median(se))

#1.constant returns to scale 
#2. growth in return to scale 

#we want to find out wich farm has the biggest return to s
scale<-data.frame(Country, Region, se)
scale[which.max(scale[,3]),]
#wehn we have increasing returns to scale -> means that
#the farms have a lot of potential to grow 

scale[which.min(scale[,3]),]
#so constant return to scale means that the thuringen with the most used
#hectares is already used it potential 

#plot 
plot(se, Hectares)
text(se, Hectares, Region, cex=0.6)


#alternative functional forms 
y <- CropOutput/Hectares
xfha<-Xfert/Hectares
xpha<-Xpest/Hectares
xlha<-Xlabtotal/Hectares

#we want to use a quadratic form so we 

xfha2<-(Xfert/Hectares)^2
xpha2<-(Xpest/Hectares)^2
xlha2<-(Xlabtotal/Hectares)^2



#interaction terms test between fertilizer and pestizides 
xfp<-xfha*xpha
xpl<-xpha*xlha
xfl<-xfha*xlha

Quad.Profu.ha<-lm(y~xfha+xpha+xlha+xfha2+xpha2+xlha2+xfp+xfl+xpl)
summary(Quad.Profu.ha)

#wich hypothesis fits better 
# 1. Option is ANOVA 

#we need to make linear prod.f. per hectar first 
Linear.Profu.ha<-lm(y~xfha+xpha+xlha)
summary(Linear.Profu.ha)

anova(Linear.Profu.ha, Quad.Profu.ha)

#Ho is 2 models are equal-> explain the same 
# p value is not smaller then 5. -> we can not reject H0
#that they explain the same 
# we chose the d=first mpdell becouse the second modell
#does not explain more ( higher complexity) 
# we dont want the higher complexity 


#here is another method to compare 2 different modells ????
install.packages("lmtest")  # Install the lmtest package
library(lmtest)              # Load the lmtest package

lrtest(Linear.Profu.ha, Quad.Profu.ha)

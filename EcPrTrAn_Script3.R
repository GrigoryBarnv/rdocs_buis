#clean the environment 
rm(list = ls())

#set the working directory
setwd("/Users/aylatan/Downloads")

library(readxl)

#read csv data
data2 <- read.csv2("FADN_grain_producers_2012.csv")

library(dplyr)

attach(data2) 

names(data2)

head(data2)
summary(data2)

table(Country)

table(Region)

#extract individual parts instead of attatch 
Y <- data2$CropOutput






#We can plot a histogram of the Y- Object; the units are Euro. For inputs the units are 68 
#physical quantities: 
options(scipen=16)
hist(Y, main="Crop Output for Spezialized Cereal Producers \n in aalible EU regioens, 2012")

#The histogram shows us that in 2012, very few farms (represented by the regional 
#averages) had crop output worth 600.000 Euro or more. Likely, these are the largest farms. 
#We can test this assumption by another plot:
plot(Hectares, CropOutput)
text(Hectares, CropOutput, Region)

#The plot shows us that the specialized cereal producers in the sample are represented by an 81 
#average farm size of 600 ha in Thüringen, and 400-500 ha in Meck.-Pom. and S-Anhalt, 82 
#respectively. 


#We can ask R to generate Average Products (AP) for each input factor, and we can list 87 
#e.g. the output of each regional farm per hectare: 
APland<-CropOutput/Hectares
APfert <- CropOutput/Xfert
APpest <- CropOutput/Xpest
APlab <- CropOutput/Xlabtotal
APseed <- CropOutput/Xseeds
APland

boxplot(APland)
boxplot(APland ~ Country)

##Does the APland increase or decrease if farms are larger, or does the APland rather depend
 #on the geographical region? We can test this by another plot. This time, we need a factor
 #101 separation by “Country” (Note that separating by region would produce one plot for each
  #                           102 observation):
  
library(lattice)
xyplot(APland ~ Hectares|Country, type=c("p", "r"), scales="free")#we ask for points and a regression kine in each plot

stripParms <- list(cex=0.6, lines=2) #formating of the headlines
xyplot(APland ~ Hectares|Country,
       type=c("p","r"),scales="free", par.strip.text = stripParams)



#We can now calculate the Mean AP per Country. Therefore, we bind the APland to our
#126 data.frame. To use the pipe, %>%, we need to install and/or load the package “dplyr”

data2<-cbind(data2,APland)
library(dplyr)
library(dplyr)

data2 %>%
  group_by(Country) %>%
  summarise(
    mean_APland = mean(APland, na.rm = TRUE), 
    median_APland = median(APland, na.rm = TRUE),
    .groups = 'drop'  # Optional, um die Gruppen nach der Zusammenfassung zu entfernen
  )

#Partial productivity measures tend to be misleading. A more useful concept is total factor
#155 productivity (TFP). In it’s simplest form, it can be measured as follows:

TFP <- CropOutput/(Hectares + Xseeds + Xfert + Xpest + Xother + Xlabtotal)

par(mfrow=c(2,2))
plot(TFP ~ APland, cex=0.5) plot(TFP ~ APfert, cex=0.5) plot(TFP ~ APpest, cex=0.5)
plot(TFP ~ APlab, cex=0.5)

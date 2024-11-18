
rm(list=ls())

library(readxl)

setwd("/Users/aylatan/Downloads")

MarketPower <- read_excel("MarketPower.xlsx")




# use MarketPower.xlsx

summary(MarketPower)

LI = lm(Lerner_index~age+AssetsthEUR+ExportrevenuethEUR+RevGR, dat=MarketPower)
summary(LI)

MU = lm(Markup~age+AssetsthEUR+ExportrevenuethEUR+RevGR, dat=MarketPower)
summary(MU)

MU1 = lm(Markup~age+AssetsthEUR+ ROA+ExportrevenuethEUR+RevGR, dat=MarketPower)
summary(MU1)

MarketPower$size2 <- MarketPower$AssetsthEUR*MarketPower$AssetsthEUR

MU2 = lm(Markup~age+AssetsthEUR+ size2 +ROA+ExportrevenuethEUR+RevGR, dat=MarketPower)
summary(MU2)

MU3 = lm(Markup~age+AssetsthEUR+ size2 +ROA+Exportdummy+RevGR, dat=MarketPower)
summary(MU3)

MU4 = lm(Markup~+age+AssetsthEUR+ ROA+Exportdummy*AssetsthEUR+RevGR, dat=MarketPower)
summary(MU4)

#Perfect multicollinearity 
  #create a dummy =1 for the opposite category of non-exporters
MarketPower$Nonexportdummy <- -1*(MarketPower$Exportdummy-1)
summary(MarketPower)
MU5 = lm(Markup~Nonexportdummy, dat=MarketPower)
summary(MU5)

#Interaction between two numerical variables

MU6 = lm(Markup~+AssetsthEUR*age, dat=MarketPower)
summary(MU6)
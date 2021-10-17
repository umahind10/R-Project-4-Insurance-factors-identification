#install.packages("rea")
install.packages('knitr')
install.packages("markdown")
install.packages("rmarkdown")


install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("contribution")
library(rmarkdown)
library(knitr)
#library(readxl)
Insdata <- read.csv("C:\\Users\\HP\\Desktop\\R Projects\\4_Insurance-Analysis-master\\SwedishMotorInsurance.csv")
View(Insdata)

dim(Insdata)

#--------------------------------------------

summary(Insdata)
#--------------------------------------------

cor(Insdata$Claims,Insdata$Payment) #--high +ve correlation

cor(Insdata$Insured,Insdata$Payment) #--high +ve correlation

plot(Insdata$Insured,Insdata$Payment) 

plot(Insdata$Claims,Insdata$Payment) 

#----------------------------------------------

lineModel <- lm(Payment ~ ., data = Insdata)
summary(lineModel)

#----------------------------------------------

?apply
ZoneResult <- apply(Insdata[,c(5,6,7)],2, function(x)tapply(x, Insdata$Zone, mean))
ZoneResult

KmResult <- apply(Insdata[,c(5,6,7)],2, function(x)tapply(x, Insdata$Kilometres, mean))
KmResult

BonusResult <- apply(Insdata[,c(5,6,7)],2, function(x)tapply(x, Insdata$Bonus, mean))
BonusResult

#----------------------------------------------------

md <- lm(Insdata$Claims ~ Insdata$Kilometres + Insdata$Zone + Insdata$Bonus + Insdata$Make + Insdata$Insured) 
options(scipen=999)
summary(md) 
render("C:\\Users\\HP\\Desktop\\R Projects\\4_Insurance-Analysis-master\\Insurance_Project.R","pdf_document")
# rmarkdown::render("C:\\Users\\HP\\Desktop\\R Projects\\4_Insurance-Analysis-master\\Insurance_Project.R", "pdf_document")



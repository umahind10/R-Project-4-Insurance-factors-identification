install.packages("readxl")
install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("caTools")
install.packages("e1071")
install.packages("rmarkdown")
library(rmarkdown)
library(readxl) 
library(plyr) 
library(caTools)
library(e1071) 
 
data<-read_excel("C:\\Users\\HP\\Desktop\\R Projects\\3-Webdata project\\1555058318_internet_dataset.xlsx")
summary(data)

cor(data$Uniquepageviews,data$Visits)

anov<-aov(Exits~.,data=data)
summary(anov)

anov2<-aov(Timeinpage~.,data=data)
summary(anov2)

data$Bounces=data$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data=data,family = "binomial")
summary(rmm)

rmarkdown::render("C:\\Users\\HP\\Desktop\\R Projects\\3-Webdata project\\Web_Data_Analysis.R")
rmarkdown::render("C:\\Users\\HP\\Desktop\\R Projects\\3-Webdata project\\Web_Data_Analysis.R", "pdf_document")

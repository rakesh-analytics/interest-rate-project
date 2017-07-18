#remove all objects from R
rm(list=ls(all = TRUE))

#set current working directory
setwd("D:/rakesh/data science/Project/Interest rate/Major Project")

#load data into R
data = read.csv("Data for Cleaning & Modeling.csv", header = T)

#Data Pre Processing
str(data)
summary(data)

#Removing the percentage
data$X1 = gsub("%","",data$X1)
data$X30 = gsub("%","",data$X30)


#Replacing missing values with NA
data = data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))

#Total missing values in data
sum(is.na(data))

#Data frame of count of missing values for each column
MissingData = data.frame(varaibles = colnames(data), 
                         MissingInfo = apply(data,2,function(x)sum(is.na(x))))


#converting Variables to their respective class
#First converting to character as its not conerting directly

for (i in c(1:6,10,13,16,18,22:31)){
  data[,i] = as.character(data[,i])
}


for (i in c(1:6,13,21,22,24:31)){
  data[,i] = as.numeric(data[,i])
}


#Feature Engineering 
data$credit_limit = (data$X29/data$X30)*100

#Binning Data

ownership = c('NONE', 'ANY',"", NA)
data[,12] = as.character(data[,12])
data$X12[data$X12 %in% ownership] = 'OTHER'

data$Interest_rate[data$X1<12] = "Low"
data$Interest_rate[data$X1>12 & data$X1<19] = "Medium"
data$Interest_rate[data$X1>19] = "High"

data$Interest_rate = as.factor(data$Interest_rate)

data$X12 = as.factor(data$X12)

data$Annual_income[data$X13<=43000] = "Low Income"
data$Annual_income[data$X13>43000 & data$X13<=80000] = "Middle Income"
data$Annual_income[data$X13>80000] = "High Income"

data$Annual_income = as.factor(data$Annual_income)

#Remove outliers using boxplot
for (i in c(1,13,29,30,31,33)){
  boxplot(data[,i])
  data = data[-which(data[,i] %in% boxplot.stats(data[,i])$out),]
}

#Removing Incomplete Observations
data = data[complete.cases(data$X1),]

data = data[complete.cases(data$X8),]

data = data[complete.cases(data$X13),]

data = data[complete.cases(data$credit_limit),]

#checking Correlation

corr = data.frame(format(cor(data[,c(1,4:6,13,21,22,24,27,28:31,33)]), digits = 2))

#checking multicollinearity
library(usdm)
vif(data[,c(1,4:6,13,21,22,24,27,28:31,33)])
vifcor(data[,c(1,4:6,13,21,22,24,27,28:31,33)], th=0.5)

#After testing for multicollinearity X4, X5 & X6
#So they are not fed to the model.

save.image("process")

#remove all objects from R
rm(list=ls(all = TRUE))

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
#First converting to character as its not converting directly

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
vifcor(data[,c(1,4:6,13,21,22,24,27,28:31,33)], th=0.9)


save.image("process")
load("process")



#Removing unnecessary variables
#data = subset(data, select = -c(X2,X3,X19,X25,X26))


library('ggplot2')
library('ggthemes')
library(gridExtra)
library(lattice)

pl = function(data,var,X,title){
 p = ggplot(aes_string(x=var),data = data )+
      geom_histogram(color =I('black'),fill = I('#099009')) +
      labs(x = X ) +
      ggtitle(title)
      return(p)
}

bar = function(dat,X,Y,Name,title){
  p = ggplot(data=dat, aes_string(x = X, fill = Y)) +
    geom_bar(stat = 'count', position = 'dodge') + labs(x = Name) +
    ggtitle(title) + theme(text=element_text(size=10))
  return(p)
}

line = function(dat,X,Y,title,nameX,nameY){
  l =  ggplot(aes_string(x=X,y=Y), data = dat) +
    geom_line(aes(color = 'red'), stat='summary',fun.y=median) +
    labs(x = nameX , y = nameY) + ggtitle(title)
  return(l)
}



p1=pl(data,"X1","Interest Rate",'Distribution of Interest rate on Loan')

p4=pl(data,"X4","Amount Requested Loan",'Distribution of Loan Amount Requested')

p5=pl(data,"X5","Amount Funded Loan",'Distribution of Loan Amount Funded')

p6=pl(data,"X6","Investor Funded Loan",'Distribution of Investor Funded Loan')

grid.arrange(p1,p4,p5,p6,ncol=2)

p13=pl(data,"X13","Annual Income",'Distribution of Annual Income')
cl=pl(data,"credit_limit","Credit Limit",'Distribution of Credit Limit')
grid.arrange(p13,cl,ncol=2)



b8=bar(data,"X8","Interest_rate","Loan Grade","Loan Grade VS Interest Rate")
b9=bar(data,"X9","Interest_rate","Loan Subgrade","Loan Subgrade VS Interest Rate")
grid.arrange(b8,b9,ncol=1)


b11I=bar(data,"X11","Interest_rate","Employed Time Period","No. of years of Enploymnet VS Interest Rate")
b11A=bar(data,"X11","Annual_income","Employed Time Period","No. of years of Enploymnet VS Interest Rate")
grid.arrange(b11I,b11A,ncol=1)


b17A=bar(data,"X17","Interest_rate","Loan Category as Provided by Borrower","Loan Category VS Interest Rate")
b17B=bar(data,"X17","Sentiment","Loan Category as Provided by Borrower","Loan Category VS Sentiment")
bar(data,"Sentiment","Interest_rate","Sentiments","Sentiments VS Interest Rate")
grid.arrange(b17A,b17B,ncol=1)



f7=ggplot(data, aes(X13, colour = X7)) +
  geom_freqpoly(binwidth = 2000) +
  labs(x = 'Annual Income') +
  ggtitle('Annual Income across payment types')

b7=bar(data,"X7","Interest_rate","No. of Payments","Payment VS Interest Rate")

grid.arrange(b7,f7,ncol=1)


remove(p1,p4,p5,p6,p13,cl,b8,b9,b11I,b11A,b17A,b17B,f7,b7)

line(data,"X1","X13","Interest Rate across Annual Income","Interest Rate","Annual Income")

line(data,"X27","credit_limit","Relation B/W Interest Rate & Open Credit Line","Open Credit Lines","Credit Limit")
line(data,"X27","X1","Relation B/W Interest Rate & Open Credit Line","Open Credit Lines","Interest Rate")

line(data,"X30","X1","Interest Rate across Annual Income","Interest Rate","Annual Income")

line(data,"X4","X13","Loan Amount across Annual Income","Loan Amount Requested","Annual Income")


bar(data,"X14","Interest_rate","Income Verification","Relation B/W Income Verification  & Interest Rate")

ggplot(aes(x=X13,y=X1),data = data) + 
  geom_jitter(aes(color = X7,bg = X7),alpha=1/10,pch=21,cex=4)+
  scale_color_brewer(type = 'div')+
  ggtitle('Annual Income and Interest Rate Relationship')


table(data$Annual_income) #Income Groups
49501/208306*100  #Percentage of High Income Group
52785/208306*100  #Percentage of Low Income Group
106020/208306*100  #Percentage of Middle Income Group


table(data$X14)
table(data$X14,data$Interest_rate)
3887/70904*100  #Percentage of Not Verified Income having High Interest
14131/74315*100 #Percentage of Verified Income having High Interest
8221/63087*100  #Percentage of Verified Income Source having High Interest

31216/70904*100  #Percentage of Not Verified Income having Low Interest
17281/74315*100 #Percentage of Verified Income having Low Interest
18816/63087*100  #Percentage of Verified Income Source having Low Interest

#Text Minning
#load libraries
library("stringr")
library(tm)
library("wordcloud2")
library("wordcloud")
library(slam)
library(sentimentr)

#Load comments/text
reason = data.frame(reason_for_loan = data$X16)


#Delete the leading spaces
reason$reason_for_loan = str_trim(reason$reason_for_loan)

##Pre-processing
#convert comments into corpus
reasonCorpus = Corpus(VectorSource(reason$reason_for_loan))
writeLines(as.character(reasonCorpus[[1]]))

#case folding
reasonCorpus = tm_map(reasonCorpus, tolower)

#remove stop words
reasonCorpus = tm_map(reasonCorpus, removeWords, stopwords('english'))

#remove punctuation marks
reasonCorpus = tm_map(reasonCorpus, removePunctuation)

#remove numbers
reasonCorpus = tm_map(reasonCorpus, removeNumbers)

#remove unnecesary spaces
reasonCorpus = tm_map(reasonCorpus, stripWhitespace)

#convert into plain text
reasonCorpus = tm_map(reasonCorpus, PlainTextDocument)

#create corpus
reasonCorpus = Corpus(VectorSource(reasonCorpus))

##wordcloud
#Remove the defined stop words
reasonCorpus_WC = reasonCorpus
reasonCorpus_WC = tm_map(reasonCorpus, removeWords, c('i','its','it','us','use','want','added','used','using','will','yes','say','can','take','one',
                                                      stopwords('english')))

#Word cloud
wordcloud(reasonCorpus_WC, max.words = 150, scale=c(5, .2), colors=brewer.pal(8, "Dark2"))


#Build document term matrix
tdm = TermDocumentMatrix(reasonCorpus)

#calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq = as.matrix(words_freq)
words_freq = data.frame(words_freq)
words_freq$words = row.names(words_freq)
words_freq = words_freq[,c(2,1)]
row.names(words_freq) = NULL
names(words_freq) = c("Words", "Frequency")
#wordcloud(words_freq$Words, words_freq$Frequency)

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)

#sentiment Analysis
library(RSentiment)
df = calculate_sentiment(reason$reason_for_loan)


#Merging the sentiments with data
data$Sentiment = df$sentiment

#Text Minning on Negative Comments
negative_sentiment = df[which(df$sentiment == 'Very Negative' | df$sentiment=='Negative'),]
#Wordcloud for negative terms
negCorpus = Corpus(VectorSource(negative_sentiment$text))
negCorpus = tm_map(negCorpus, tolower)
negCorpus = tm_map(negCorpus, removeWords, c('will','can','thanks','better','get','also',
                                             'well','good','now', stopwords('english')))
negCorpus = tm_map(negCorpus, removePunctuation)
negCorpus = tm_map(negCorpus, removeNumbers)
negCorpus = tm_map(negCorpus, stripWhitespace)
negCorpus = tm_map(negCorpus, PlainTextDocument)
negCorpus = Corpus(VectorSource(negCorpus))

wordcloud(negCorpus, max.words = 200, scale=c(3, .1), colors=brewer.pal(6, "Dark2"))


save.image("word")
load("word")


#Random Forest Model
library(randomForest)
library('dplyr')
library("caret")

#divide the data into train and test
# to get same data in each time
set.seed(123) 

train = data[sample(nrow(data), 20000, replace = F), ]

test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

rf_model <- randomForest(X1 ~ X4 + X8 + X9 + X13 + X21 + X22 + X24+ X27 + X28 + X29 + X30 +
                              X31 + credit_limit , data = train, 
                              importance = TRUE ,  ntree = 100)


## Predict using the test set
prediction = predict(rf_model, test)
importance = importance(rf_model, type = 1)

#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

1-mape(test[,1], prediction)

# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance = round(importance[ , '%IncMSE'], 2))

# Create a rank variable based on importance
rankImportance = varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))




# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 6, colour = 'yellow') +
  labs(x = 'Variables') + coord_flip() + theme_few()





###Linear regression model with the same pre processing techniques
lm_model = lm(X1 ~ X4 + X9 +X13 + X21 + X22 + X24+ X27 + X28 + X29 + X30 +
                X31 + credit_limit, data = train)
summary(lm_model)
pred = predict(lm_model, test[,c(4,9,12,13,21,22,24,27:29,30,31,33)])

#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

1-mape(test[,1], pred)

#extract coefficients from lm
coeff = data.frame(Coeff = lm_model$coefficients, StdError = summary(lm_model)$coefficients[, 2],
                   tValue = summary(lm_model)$coefficients[, 3],
                   Pval = summary(lm_model)$coefficients[, 4])
coeff$variables = row.names(coeff)
coeff = coeff[,c(5,1:4)]
rownames(coeff) = NULL
write.csv(coeff, "Coefficients_stats.csv", row.names = F)

save.image("model")
load("model")




#Applying model on Holdout for test file.
holdout = read.csv("Holdout for Testing.csv", header = T)

#Data Pre Processing
str(holdout)

holdout$X30 = gsub("%"," ",holdout$X30)
holdout$X4 = gsub('\\$',"",holdout$X4)
holdout$X5 = gsub('\\$',"",holdout$X5)
holdout$X6 = gsub('\\$',"",holdout$X6)


#Replacing missing values with NA
holdout = data.frame(apply(holdout, 2, function(x) gsub("^$|^ $", NA, x)))
sum(is.na(holdout))

#Store Values in data frame
Missingdata1 = data.frame(varaibles = colnames(holdout), 
                         MissingInfo = apply(holdout,2,function(x)sum(is.na(x))))

#converting Variables,

for (i in c(1:6,10,13,16,18,22:31)){
  holdout[,i] = as.character(holdout[,i])
}

library(taRifx)

for (i in c(1:6,13,21,22,24:31)){
  holdout[,i] = destring(holdout[,i])
}


#Feature Engineering 
holdout$credit_limit = (holdout$X29/holdout$X30)*100

#Binning holdout

ownership = c('NONE', 'ANY',"", NA)
holdout[,12] = as.character(holdout[,12])
holdout$X12[holdout$X12 %in% ownership] = 'OTHER'

holdout$X12 = as.factor(holdout$X12)


#Remove outliers using boxplot
for (i in c(13,29,31,30,33)){
  boxplot(holdout[,i])
  holdout = holdout[-which(holdout[,i] %in% boxplot.stats(holdout[,i])$out),]
}

#Removing Incomplete Observations

holdout = holdout[complete.cases(holdout$X30),]

holdout = holdout[complete.cases(holdout$credit_limit),]


rf_prediction = predict(rf_model, holdout[,-1])

#Merging the predicted values with the holdout test file
holdout$X1 = rf_prediction
write.csv(holdout, file = 'rf_solution.csv',row.names = F)


lm_prediction = predict(lm_model, holdout[,c(4,9,13,21,22,24,27:29,30,31,33)])
holdout$X1 = lm_prediction
write.csv(holdout, file = 'lm_solution.csv',row.names = F)


#Save model
save.image("majorproject")
load("majorproject")

remove(df,reason,reasonCorpus,reasonCorpus_WC,i,negCorpus,
       pal2,Missingdata1,MissingData,tdm,pred,prediction,test,train,negative_sentiment,words_freq)





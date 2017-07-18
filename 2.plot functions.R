load("process")

#Loading libraries to draw plots
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
3887/70904*100  #Percentage of Non Verified Income having High Interest
14131/74315*100 #Percentage of Verified Income having High Interest
8221/63087*100  #Percentage of Verified Income Source having High Interest

31216/70904*100  #Percentage of Non Verified Income having Low Interest
17281/74315*100 #Percentage of Verified Income having Low Interest
18816/63087*100  #Percentage of Verified Income Source having Low Interest

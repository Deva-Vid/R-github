nba = read.csv("https://storage.googleapis.com/dimensionless/Analytics/NBA_train.csv")
nbat =read.csv("https://storage.googleapis.com/dimensionless/Analytics/NBA_test.csv")
str(nba)
str(nbat)
View(nba)
table(w=nba$W,playoffs=nba$Playoffs)
nba$pt <- nba$PTS-nba$oppPTS 
nbat$pt <- nbat$PTS-nbat$oppPTS
model1 =lm(W~pt,data=nba)
model1
predict1 = predict(model1,newdata = nbat)
summary(model1)
# 1-SST/SSE
m <- (nbat$W-predict1)
sse = sum(m^2)
sse
k <- mean(nbat$W)
sst = sum((nbat$W-k)^2)
sst
1-sse/sst
model2 = lm(PTS~ FG + X2P + X3PA +FT + FTA,data=nba)
summary(model2)
model2 = lm(PTS~ FG + X2P +FT,data=nba)
summary(model2)
vif(model2)
model3 = lm(PTS~ FG + X3P +FT,data=nba) #  TO DECIDE RIGHT MODEL CHECK VIF
summary(model3)
vif(model3)
predict1 = predict(model3,newdata = nbat)
# 1-SST/SSE
m <- (nbat$PTS-predict1)
sse = sum(m^2)
sse
k <- mean(nbat$PTS)
sst = sum((nbat$PTS-k)^2)
sst
1-sse/sst

# Forecasting Elantra sales
elantra = read.csv("https://storage.googleapis.com/dimensionless/Analytics/elantra.csv")
elantra
View(elantra)
eltrain = subset(elantra, Year < 2013)
eltrain
dim(eltrain)
eltest = subset(elantra,Year >2012)
eltest
dim(eltest)
table()
model31 = lm(ElantraSales ~ Month + Year + Unemployment + Queries + CPI_energy + CPI_all,data = eltrain)
model31
summary(model31)
model32 = lm(ElantraSales ~ Unemployment + Queries +CPI_energy + CPI_all,data = eltrain)
model32
summary(model32)
model33 = lm(ElantraSales ~ Unemployment + Queries +CPI_energy + CPI_all + Month,data = eltrain)
model33
summary(model33)
model34 = lm(ElantraSales ~ Unemployment + Queries +CPI_energy + CPI_all+ as.factor(Month),data = eltrain)
model34
summary(model34)
cor(eltrain[,1:7])
# one hot encoding

# model 2 :classicfication alogritham (logistic regression)
quality = read.csv("https://storage.googleapis.com/dimensionles
s/Analytics/quality.csv")
quality
View(quality)
str(quality)
table(quality$PoorCare) # to check goodcare and badcare among poorcare column 
# base line case for classification 

#load "caTools" package
set.seed(88) # to fix the seed for algorithm 
split = sample.split(quality$PoorCare,SplitRatio = 7/10) # to maintaain ratio in poor care & good care
split

split
table(split)
#subsetting
qtest <- subset(quality,split==FALSE)
qtrain <- subset(quality,split==TRUE)
qtest
qtrain
str(qtest)
str(qtrain)
table(qtest$PoorCare)
table(qtrain$PoorCare)
qlog <- glm(PoorCare ~ OfficeVisits + Narcotics,data = qtrain, family = binomial )
qlog
summary(qlog)# AIC less then model is good
# p(y=1) i.e probablity of poorcare 1= is for bad , 0 = for good
qlog$fitted.values 
ptrain<-predict(qlog,type = "response") # probablity of poor care 
ptrain
tapply(ptrain,INDEX = qtrain$PoorCare,FUN = mean)    
tapply(ptrain,INDEX = qtrain$PoorCare,FUN = summary)    
qlog1 <- glm(PoorCare~ ProviderCount + StartedOnCombination,data = qtrain, family = binomial )
qlog1 
summary(qlog1)
ptrain1 <- predict(qlog1,type ="response")
ptrain1
#11/03/2017
conf_matrix = table(qtrain$PoorCare,ptrain >= 0.5)
conf_matrix
colnames(conf_matrix) <- c("predic=0","predict=1")
conf_matrix

65/69

9/23
74/92
conf_matrix1 = table(qtrain$PoorCare,ptrain >= 0.2)
conf_matrix1
#sensitivity
17/23
#specificity
47/69
#accuracy
62/92
conf_matrix2 = table(qtrain$PoorCare,ptrain >= 0.7)
conf_matrix2
#sensitivity
5/23
#specificity
68/69
#accuracy
73/92
# ROCR PACKAGE
library(help =ROCR)
rocrpred <- prediction(ptrain,qtrain$PoorCare)
rocrpred
rocrperf <- performance(rocrpred,"tpr","fpr")
rocrperf
plot(rocrperf,colorize = TRUE) 
plot(rocrperf,colorize = TRUE,print.cutoffs.at= seq(0,1,0.1),text.adj=c(-0.2,1.7)) 

predicttest2 = predict(qlog,type = "response",newdata = qtest)
predicttest2
conf_matrix3 = table(qtest$PoorCare,predicttest2 >= 0.3)
conf_matrix3
#sensitivity
8/10
#specificity
22/39
rocrpred1 <- prediction(predicttest2,qtest$PoorCare)
auc = performance(rocrpred1,"auc")
auc
auc@y.values

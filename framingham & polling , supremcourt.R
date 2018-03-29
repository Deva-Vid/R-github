framingham = read.csv("https://storage.googleapis.com/dimensionless/Analytics/framingham.csv")
framingham
framingham <- na.omit(framingham)
View(framingham)
table(framingham$TenYearCHD)
train
set.seed(1000)
split = sample.split(framingham$TenYearCHD,SplitRatio = 0.65)
split
ftrain <- subset(framingham,split == TRUE)
ftest <-  subset(framingham,split == FALSE)
str(ftrain)
str(ftest)
dim(ftrain)
dim(ftest)
summary(ftest)
flog = glm(TenYearCHD ~ .,data = ftrain,family = binomial)
summary(flog)
cor(ftrain[,1:15])
# load car package
vif(flog)
summary(flog)
flog2 = glm(TenYearCHD ~  age + cigsPerDay + education + male + sysBP +glucose +totChol,data = ftrain,family = binomial)
flog2
pftrain<- predict(flog2,type = "response")
pflog
conf_matrix1 = table(ftrain$TenYearCHD,pftrain >= 0.5)
conf_matrix1
23/(339+23) #sensitivity
2003/(2003+13) # specifcity
(2003+23)/(2003+23+13+339) # accuracy
table(ftrain$TenYearCHD) # base line accuracy
 2016/(2016+362)
 frocrpred1 <- prediction(pftrain,ftrain$TenYearCHD)
 fperfo <- performance(frocrpred1,"tpr","fpr")
 plot(fperfo)
 plot(fperfo,colorize = TRUE)
 auc = performance(frocrpred1,"auc")
 auc
 auc@y.values
 conf_matrix2 = table(ftrain$TenYearCHD,pftrain >= 0.2)
 conf_matrix2
 207/(207+155) # sens
 1593/(1593+423) #specify
 (1593+207)/(1593+423+155+207)
 
 conf_matrix3 = table(ftrain$TenYearCHD,pftrain >= 0.7)
 conf_matrix3
 4/(207+155) # sens
 2015/(1593+423) #specify
 (2015+4)/(1593+423+155+207)
 
 pftest <- predict(flog2,type = "response",newdata = ftest)
 conf_matrix3 = table(ftest$TenYearCHD,pftest >= 0.2)
 conf_matrix3
 99/(96+99) # sense
 837/(837+248) # specif
 frocrpred2 <- prediction(pftest,ftest$TenYearCHD)
 fperfo2 <- performance(frocrpred2,"tpr","fpr")
 plot(fperfo2)
 plot(fperfo2,colorize = TRUE)
 auc = performance(frocrpred2,"auc")
 auc@y.values
 # p value less than .05 to consider variable is significant
 chisq.test(ftrain$male,ftrain$TenYearCHD)
 t.test(age~TenYearCHD,data = ftrain)
 chisq.test(ftrain$education,ftrain$TenYearCHD)
 chisq.test(ftrain$currentSmoker,ftrain$TenYearCHD)
 t.test(glucose~TenYearCHD,data= ftrain)
 
 # polling
 polling <- read.csv("https://storage.googleapis.com/dimensionless/Analy
tics/PollingData.csv")
 View(polling)
 str(polling)
 table(polling$State)
 summary(polling)
 # load mice package

 simple <- polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
 simple
 md.pattern(simple)
 imp_mice <- mice(simple,seed = 144)
 imp_mice
 imp_mice$imp$Rasmussen  # GENRATE MISSING VALUE TO FILL NA/MISSING VALUES
 imp_mice$imp$SurveyUSA
 # LOAD LATTICE PACKAGE FOR STRIPPLOT
 stripplot(imp_mice,pch=21,cex=0.6)
 imputed <- complete(imp_mice,1) # to fill NA/MISSING  values with mice function
 imputed
summary(imputed)
# replacing polling rasmussen & sirveyUSA replaced by new values
polling$Rasmussen <- imputed$Rasmussen 
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)
atrain <- subset(polling,Year >= 2004 & Year <= 2008)
atest <-  subset(polling,Year >= 2012)
atrain
atest
dim(atest)
dim(atrain)
summary(polling)
View(polling)
#smart base line model
table(atrain$Republican,sign(atrain$Rasmussen))
# will get confusion ,matrix  2X3 column data m(0,1) X n(-1,0,1)
#accuracy
(42+52)/(42+1+4+52+1)
cor(polling[2:7])
# always prediction on train data in  model 
emodel <- glm(Republican ~ PropR ,data = atrain ,family = binomial)
emodel
etrain<- predict(emodel,type = "response")
econf_matrix <- table(atrain$Republican,etrain >= 0.5)
econf_matrix
(45+51)/(51+45+1+1)
# for test data prediction 
eftest1 <- predict(emodel,type = "response",newdata = atest)
econf_matrix1 <- table(atest$Republican,eftest1 >= 0.5)
econf_matrix1
(23+21)/(23+21+1) # accuracy
subset(atest,eftest1 > 0.5 & Republican == 0)
# we predicted 44 state accurate with 1 false state prediction i.e florida
# Decision Tree 18/3/2017
# load ILSR package
data("Hitters")
summary(Hitters)
View(Hitters)
# removing NA's from salary
Hitters <- na.omit(Hitters)
dim(Hitters)
# converting salary into log scale
Hitters$Salary <- log(Hitters$Salary)
summary(Hitters)
View(Hitters)
hist(Hitters$Salary) # ploting log salary
set.seed(3000)
split1 <-  sample.split(Hitters$Salary,SplitRatio = 0.8)
dtrain <- subset(Hitters,split1 == TRUE)
dim(dtrain)
dtest <-  subset(Hitters,split1 == FALSE)
dim(dtest)
#load library rpart and rplot
smodel <- rpart(Salary ~ Hits + Years,data =  dtrain,minbucket = 5,cp = 0.12)
smodel
summary(smodel)
# load rpart model
prp(smodel) # to plot tree 
rpart.plot(smodel)
#19/03/2017
stevens = read.csv("https://storage.googleapis.com/dimensionless/An
alytics/stevens.csv")
stevens
summary(stevens)
dim(stevens)
#load ca tools package for sample split
split2 <- sample.split(stevens$Reverse ,0.7)
set.seed(3000)
strain <- subset(stevens, split2 == TRUE)
stest <-  subset(stevens,split2 == FALSE)
strain
dim(strain)
dim(stest)
View(stevens)
# model always on train , prediction can be on test and train
stev_tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst ,data = strain ,minbucket = 25 ,method = "class")
stev_tree
summary(stev_tree)
rpart.plot(stev_tree)
prp(stev_tree)
predictcart <- predict(stev_tree,newdata = stest ,type = "class")
predictcart
table(stest$Reverse,predictcart)
(59+64)/(35+42+24+69)
predictcart2 <- predict(stev_tree,data = strain ,type = "class")
table(strain$Reverse,predictcart2)
(112+143)/(112+68+72+143)
predictroc <- predict(stev_tree,newdata = stest )
predictroc
rocpred <- prediction(predictroc[,2],stest$Reverse)
rocpred
rocperfo <- performance(rocpred,"auc")
rocperfo@y.values
#25/03/2017
numFolds<-trainControl(method = "cv",number = 10)
numFolds
cpGrid<-expand.grid(cp=seq(0.01,1,0.01))
class(cpGrid)
cpGrid
modelcv<-train(as.factor(Reverse)~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=strain,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
modelcv
plot(modelcv)
stev_treecv <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst ,data = strain ,minbucket = 25 ,method = "class",cp=0.19)
stev_treecv
plot(stev_treecv)
pred_cv<-predict(stev_treecv,newdata=stest,type = "class")
pred_cv

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
 
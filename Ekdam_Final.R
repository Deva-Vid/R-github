# setwd('D:/DEv/Data Set')
setwd('D:/DEv/R  Project/Data Set')
datatest=read.csv("Group Project on R-Data Set-3.csv")
datatrain=read.csv("Group Project on R-Data Set-4.csv")

# dim(datatest)
# dim(datatrain)

datatest$SalePrice=NA

x_all=rbind(datatrain,datatest)

# checking for number of NAs for each variable
colSums(is.na(x_all))
# checking percentage for number of NAs for each variable
Percent=(colSums(is.na(x_all))/2919)*100
#Percent=sapply(x_all, function(x) sum(is.na(x)/2919)*100)
Percent

#removing variable with more than 40% NA's
x_all$Alley <- NULL
x_all$FireplaceQu <- NULL
x_all$Fence <- NULL
x_all$MiscFeature <- NULL
x_all$PoolQC <- NULL

# summary(x_all)
x_all$LotFrontage[is.na(x_all$LotFrontage)]=median(x_all$LotFrontage,na.rm=TRUE)
hist(x_all$LotFrontage)
is.numeric(x_all$LotFrontage)
x_all$MasVnrArea[is.na(x_all$MasVnrArea)]= 0
hist(x_all$MasVnrArea)

# To get mode value for categorical variable 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(x_all$MSZoning)

x_all$MSZoning[is.na(x_all$MSZoning)]= getmode(x_all$MSZoning)

class(x_all$MSZoning)

x_all$MasVnrType[is.na(x_all$MasVnrType)]= getmode(x_all$MasVnrType)

x_all$Utilities[is.na(x_all$Utilities)]= getmode(x_all$Utilities)

x_all$Exterior1st[is.na(x_all$Exterior1st)]= getmode(x_all$Exterior1st)

x_all$Exterior2nd[is.na(x_all$Exterior2nd)]= getmode(x_all$Exterior2nd)

x_all$BsmtQual[is.na(x_all$BsmtQual)]= getmode(x_all$BsmtQual)

x_all$BsmtCond[is.na(x_all$BsmtCond)]= getmode(x_all$BsmtCond)

x_all$BsmtExposure[is.na(x_all$BsmtExposure)]= getmode(x_all$BsmtExposure)

x_all$BsmtFinType1[is.na(x_all$BsmtFinType1)]= getmode(x_all$BsmtFinType1)

colours()

x_all$BsmtFinSF1[is.na(x_all$BsmtFinSF1)]=median(x_all$BsmtFinSF1,na.rm=TRUE)
hist(x_all$BsmtFinSF1,col="tomato")
class(x_all$BsmtFinSF1)

x_all$BsmtFinType2[is.na(x_all$BsmtFinType2)]= getmode(x_all$BsmtFinType2)


x_all$BsmtFinSF2[is.na(x_all$BsmtFinSF2)]=median(x_all$BsmtFinSF2,na.rm=TRUE)
hist(x_all$BsmtFinSF2,col="turquoise1")

x_all$BsmtUnfSF[is.na(x_all$BsmtUnfSF)]=median(x_all$BsmtUnfSF,na.rm=TRUE)
hist(x_all$BsmtUnfSF,col="tan1")

x_all$TotalBsmtSF[is.na(x_all$TotalBsmtSF)]=median(x_all$TotalBsmtSF,na.rm=TRUE)
hist(x_all$TotalBsmtSF,col="turquoise3")

x_all$Electrical[is.na(x_all$Electrical)]= getmode(x_all$Electrical)


x_all$BsmtFullBath[is.na(x_all$BsmtFullBath)]= getmode(x_all$BsmtFullBath)
hist(x_all$BsmtFullBath,col="salmon2")

x_all$BsmtHalfBath[is.na(x_all$BsmtHalfBath)]= getmode(x_all$BsmtHalfBath)
hist(x_all$BsmtHalfBath,col="snow3")

x_all$Functional[is.na(x_all$Functional)]= getmode(x_all$Functional)

x_all$BsmtFinType2[is.na(x_all$BsmtFinType2)]= getmode(x_all$BsmtFinType2)

x_all$GarageYrBlt[is.na(x_all$GarageYrBlt)]=mean(x_all$GarageYrBlt,na.rm=TRUE) #

hist(x_all$GarageYrBlt,col="sienna")

x_all$GarageFinish[is.na(x_all$GarageFinish)]= getmode(x_all$GarageFinish)

x_all$GarageType[is.na(x_all$GarageType)]= getmode(x_all$GarageType)

x_all$GarageCars[is.na(x_all$GarageCars)]= mean(x_all$GarageCars,na.rm=TRUE)

x_all$GarageArea[is.na(x_all$GarageArea)]= mean(x_all$GarageArea,na.rm=TRUE)

x_all$GarageQual[is.na(x_all$GarageQual)]= getmode(x_all$GarageQual)

x_all$GarageCond[is.na(x_all$GarageCond)]= getmode(x_all$GarageCond)

x_all$SaleType[is.na(x_all$SaleType)]= getmode(x_all$SaleType)
class(x_all$SaleType)

# Ordering variable 
# ExterCond ,ExterQual,BsmtCond,BsmtQual,KitchenQual,GarageCond,HeatingQC,GarageQual

o1<-c("Po","Fa","TA","Gd","Ex")
x_all$ExterCond<-ordered(x_all$ExterCond,levels = o1 )
x_all$ExterQual<-ordered(x_all$ExterQual,levels = o1 )
x_all$BsmtCond<-ordered(x_all$BsmtCond,levels = o1 )
x_all$BsmtQual<-ordered(x_all$BsmtQual,levels = o1 )
x_all$KitchenQual<-ordered(x_all$KitchenQual,levels = o1 )
x_all$GarageCond<-ordered(x_all$GarageCond,levels = o1 )
x_all$HeatingQC<-ordered(x_all$HeatingQC,levels = o1 )
x_all$GarageQual<-ordered(x_all$GarageQual,levels = o1 )



o2<-c( "Unf","LwQ","Rec","BLQ","ALQ","GLQ")

x_all$BsmtFinType1<-ordered(x_all$BsmtFinType1,levels = o2)

x_all$BsmtFinType2<-ordered(x_all$BsmtFinType2,levels = o2)


# Functional
o3<-c( "Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ" )
x_all$Functional<-ordered(x_all$Functional,levels = o3)

x_all$PavedDrive<-ordered(x_all$PavedDrive,levels = c("N","P","Y"))

x_all$Utilities<-ordered(x_all$Utilities,levels =c("ELO", "NoSeWa", "NoSewr", "AllPub"))

x_all$LotShape<-ordered(x_all$LotShape,levels = c("IR3", "IR2", "IR1", "Reg"))

x_all$BsmtExposure<-ordered(x_all$BsmtExposure,levels = c("NA","No","Mn","Av","Gd" ))

x_all$LandSlope<-ordered(x_all$LandSlope,levels = c("Sev","Mod","Gtl"))

x_all$Electrical<-ordered(x_all$Electrical,levels = c("Mix","FuseP","FuseF","FuseA","SBrkr"))

x_all$GarageFinish<-ordered(x_all$GarageFinish,levels = c("Unf","RFn","Fin"))


# summary(x_all)
# View(x_all)

# Adding Numeric data column into s 
s<-x_all[,c(2,4,5,17:20,26,34,36:38,43:52,54,56,58,60,61,65:73,76)]

# Scaling Numeric data columns stored into s1
# Rescaling each column between 0 to 1
diff(range(s[,2]))
s1<- as.data.frame(apply(s[,-37], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
# View(s1)

# Replacing old data column with scaled data  column  in test data
x_all[,c(2,4,5,17:20,26,34,36:38,43:52,54,56,58,60,61,65:73)]<-s1
# Removing ID column
x_all<-x_all[,-1]
# View(x_all)

# Spliting data into xtrain and xtest based on NA values in SalesPrice
xtrain<-x_all[!is.na(x_all$SalePrice),] # for model
xtest<-x_all[is.na(x_all$SalePrice),] # for prediction


# View(xtrain) # train data
# View(xtest)  # test data


# correlation betweeen independernt variable 
#install.packages('corrplot')
library(corrplot)

# tl.col (for text label color) 
# tl.srt (for text label string rotation) are used to change text colors and rotations
# checking collinearity between numerical variable also with  target variable
s5<-xtrain[,c(1,3,4,16:19,25,33,35:37,42:51,53,55,57,59,60,64:72,75)]
# View(s5)
corrplot(cor(s5), method="number", number.cex=0.5,shade.col=NA,tl.cex = 0.5, cl.cex = 0.5, tl.col="black")

# Removing variable Highly corelated variable (>0.7) 
# xtrain$X1stFlrSF,xtrain$GarageArea,xtrain$TotRmsAbvGrd,xtrain$GarageYrBlt
xtest1<-xtest[,-c(42,53,57,60)]
xtrain1<-xtrain[,-c(42,53,57,60)]
dim(xtest1)
dim(xtrain1)

# taking columns whose P value <= 0.05 

model2<-lm(SalePrice ~ .,data = xtrain1)
h<-data.frame(summary(model2)$coef[summary(model2)$coef[,4] <= 0.05,4])
h
# View(h)
# View(xtrain1)

# Taking significant variable
# checked for collinearity column 
final_train <- xtrain1[,c(2,4,5,9:13,15:18,20,21,25,28:33,35,36,42,44,47,49,51,53,57,65,71)]
final_test <- xtest1[,c(2,4,5,9:13,15:18,20,21,25,28:33,35,36,42,44,47,49,51,53,57,65,71)]

# View(final_train)
model<-lm(SalePrice ~ .,data = final_train)
summary(model)

linear<-predict(model,newdata = final_train)
#install.packages('Metrics')
library(Metrics)
rmse(final_train$SalePrice,linear)

# predicting model on test data
pred5<-predict(model,newdata = final_test)

######################################


## # package required for the model Decision tree.
library(rpart)
library(caret)

tree<-rpart(SalePrice ~ .,data =  final_train)
summary(tree)
tpred<-predict(tree,newdata = final_train)
rmse(tpred,final_train$SalePrice)

#  predicting model on test data
tpred1<-predict(tree,newdata = final_test)

# varImp(tree)
# Ploting tree  variable based on there significace 
plot(tree,margin = 0.1)
#margin is used to adjust the size of the plot, For viewing labels
text(tree,use.n = TRUE,pretty = TRUE,cex =0.5)
####################################
# Applying Random Forest

#install.packages("randomForest")
library("randomForest")
# Random forest using bagging 
forest<-randomForest(SalePrice ~ .,data = final_train,importance =  TRUE, ntree = 500,mtry = 15)

rfpred<-predict(forest,newdata = final_train)
rmse(rfpred,final_train$SalePrice)

#  predicting model on test data
rfpred1<-predict(forest,newdata = final_test)

# ploting variable importance graph 
importance(forest)
varImpPlot(forest,cex=0.5,col = "tomato")

# Applying neural network
# install.packages("nnet")
library(nnet)

N_model<-nnet(SalePrice ~.,data =  final_train,skip= TRUE,linout = TRUE,size=3)
npred<-predict(N_model,newdata = final_train)
rmse(npred,final_train$SalePrice)

# predicting model on test data
npred1<-predict(N_model,newdata = final_test)


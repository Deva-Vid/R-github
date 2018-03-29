
stevens<-read.csv("https://storage.googleapis.com/dimensionless/Analytics/stevens.csv")
str(stevens)
# Converting Unconst and Reverse as factors.
stevens$Unconst<-as.factor(stevens$Unconst)
stevens$Reverse<-as.factor(stevens$Reverse)
str(stevens)
library(caTools)
set.seed(3000)
spl<-sample.split(stevens$Reverse,0.7)
Train<-subset(stevens,spl==TRUE)
Test<-subset(stevens,spl==FALSE)
str(Train)
str(Test)
library(rpart)
StevensTree<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",minbucket=25)
summary(StevensTree)
library(rpart.plot)
prp(StevensTree)
plot(StevensTree)
text(StevensTree,cex=0.5)
  plotcp(StevensTree)
prune.rpart(StevensTree,cp=0.036)
StevensLog<-glm( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,family=binomial)
PredictLog<-predict(StevensLog,newdata = Test,type="response")
PredictCart<-predict(StevensTree,newdata = Test,type="class")
str(PredictCart)
PredictCart
table(Test$Reverse,PredictLog>=0.5)
table(Test$Reverse,PredictCart)

(71+41)/(36+22+71+41)

PredictROC<-predict(StevensTree,newdata = Test,type="prob")
library(ROCR)
PredictROC[,2]
ROCRpred<-prediction(PredictROC[,2],Test$Reverse)
ROCRperf<-performance(ROCRpred,'auc')

plot(ROCRperf,colorize=TRUE)
tapply(PredictROC[,2],Test$Reverse,summary)

# Quick Question

as.numeric(performance(ROCRpred,'auc')@y.values)
set.seed(3000)
StevensTree2<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",minbucket=3)
StevensTree2$cptable
plotcp(StevensTree2)
Pruned_tree<-prune.rpart(tree = StevensTree2,cp=0.036)
prp(Pruned_tree)
Pruned_tree$cptable
pred_prune<-predict(Pruned_tree,newdata = Test,type="class")
table(Test$Reverse,pred_prune)
prp(Pruned_tree)
prp(StevensTree2,cex=0.5)

StevensTree3<-rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",minbucket=100)
prp(StevensTree3)
summary(StevensTree3)
# Using the tree library
library(tree)
model_tree<-tree(as.factor(Reverse) ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data = Train)
plot(model_tree)
text(model_tree,cex=0.7)
summary(model_tree)
model_tree
# Making Prediction 
pred_tree<-predict(model_tree,Test,type="class")
pred_tree
table(Test$Reverse,pred_tree)
#Pruning the tree
cv.model<-cv.tree(model_tree,FUN = prune.misclass)
cv.model
par(mfrow=c(1,2))
plot(cv.model$size,cv.model$dev,type="b")
plot(cv.model$k,cv.model$dev,type="b")
prune.tree<-prune.misclass(model_tree,best = 12)
prune.tree

plot(prune.tree)
par(mfrow=c(1,1))
text(prune.tree,cex=0.7)
pred_prune<-predict(prune.tree,Test,type="class")
table(Test$Reverse,pred_prune)
# Random Forest
library(randomForest)
set.seed(50)
StevensForest<-randomForest(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
plot(StevensForest$err.rate[,1])
class(StevensForest$err.rate)
dim(StevensForest$err.rate)
StevensForest$err.rate[200,1]
set.seed(50)
cv_rf<-rfcv(Train[,3:8],trainy = as.factor(Train$Reverse),cv.fold = 5,step=2)
plot(cv_rf$n.var,cv_rf$error.cv,type="o")
rfcv()
cv_rf$n.var
cv_rf$error.cv
plot(cv_rf)
(StevensForest)
par(mfrow=c(2,1))
plot(StevensForest)
StevensForest$err.rate
varImpPlot(StevensForest)
StevensForest$err.rate
# Converting to factors
Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)

PredictForest<-predict(StevensForest,newdata = Test)
PredictForest
table(Test$Reverse,PredictForest)

# Quick Question 
set.seed(100)
StevensForest2<-randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,nodesize=25,ntree=1000)
PredictForest2<-predict(StevensForest2,newdata = Test)
table(Test$Reverse,PredictForest2)


# Quick Question 2
set.seed(200)
StevensForest3<-randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
PredictForest3<-predict(StevensForest3,newdata = Test)
table(Test$Reverse,PredictForest3)
table(Test$Reverse,PredictForest2)

# Cross Validation 
# install.packages("caret")
library("caret")
# install.packages("ggplot2")
# install.packages("class")
# install.packages("e1071")
numFolds<-trainControl(method="cv",number = 10)
numFolds
cpGrid<-expand.grid(cp=seq(0.01,1,0.01))
class(cpGrid)
model<-train(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
model
plot(model)
prp(model$finalModel)
model$finalModel
StevensTreeCV<-rpart( Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train,method ="class",cp=0.17)
prp(StevensTreeCV)
PredictTreeCV<-predict(model,newdata = Test)
PredictTreeCV
table(Test$Reverse,PredictTreeCV)
prp(StevensTreeCV)
plot(model)
table(Test$Reverse)

tapply(Test$Reverse,Test$LowerCourt,table)
#####################

lbr<-subset(Train,Train$LowerCourt=="liberal")
table(Train$Reverse)
table(lbr$Reverse)

model<-train(as.factor(Reverse)~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="glm",trControl=numFolds)
prp(model$finalModel)
summary(model)
pred_glm<-predict(model,Test,type="prob")
table(Test$Reverse,pred_glm[,2]>=0.5)
(67+47)/170


# XGBOOST
library(xgboost)
library(Matrix)
# Creating sparse matrix
require(Matrix)
sparse_matrix<-sparse.model.matrix(Reverse~.-1-Docket-Term,Train)
sparse_matrix@Dimnames
mat<-as.matrix(sparse_matrix)
class(sparse_matrix)
dtrain<-xgb.DMatrix(data=sparse_matrix,label=Train[,9])
dtrain
# Repeating same for test data
sparse_matrix_test<-sparse.model.matrix(Reverse~.-1-Docket-Term,Test)
sparse_matrix@Dimnames
dtest<-xgb.DMatrix(data=sparse_matrix_test,label=Test[,9])
getinfo(dtrain,"label")
# Applying the model
watchlist=list(train=dtrain,test=dtest)
model_xgb<-xgb.train(data=dtrain,eta=0.01,max_depth=1,objective="binary:logistic",nrounds = 50,verbose=2,watchlist = watchlist)
class(Train$Reverse)
cv<-xgb.cv(data=dtrain,eta=0.05,max_depth=4,objective="binary:logistic",verbose=2,nfold = 10,prediction = T,nround=1000)
cv$dt
pred_xgb<-predict(model_xgb,dtest)
table(Test$Reverse,pred_xgb>=0.5)
imp<-xgb.importance(colnames(sparse_matrix),model = model_xgb)
imp
xgb.plot.tree(colnames(sparse_matrix),model = model_xgb,n_first_tree = 2)
xgb.plot.importance(imp,numberOfClusters = 1)

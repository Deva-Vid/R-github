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

flower <- read.csv("https://storage.googleapis.com/dimensionl
ess/Analytics/flower.csv",header = FALSE)
flower
str(flower)
dim(flower)
flowermatrix <- as.matrix(flower)
flowervector <- as.vector(flowermatrix)
dim(flowermatrix)
distance1<- dist(flowervector,method = "euclidean")
distance1
clusterintensity<-hclust(distance1,method = "ward.D")
clusterintensity
plot(clusterintensity)
flowercluster<-cutree(clusterintensity,k =3)
flowercluster
table(flowercluster)
tapply(flowervector,flowercluster,mean)
dim(flowercluster) =c(50,50) # 2nd method to 
mat<-matrix(flowercluster,nrow = 50,ncol = 50,byrow = F)
image(flowercluster,axes=FALSE)
image(flowercluster,axes=FALSE,col = grey(seq(0,1,length=256)))
image(flowermatrix,axes=FALSE,col = grey(seq(0,1,length=256)))

healthy<-read.csv("https://storage.googleapis.com/dime
nsionless/Analytics/healthy.csv",header=FALSE)
healthymatrix<-as.matrix(healthy)
dim(healthymatrix)
healthyvector<-as.vector(healthymatrix)
image(healthymatrix,axes=FALSE,col = grey(seq(0,1,length=256)))
k=5
set.seed = 1
KMC<-kmeans(healthyvector,centers = k,iter.max = 100)
KMC
KMC$cluster
KMC$centers
KMC$totss
KMC$withinss
KMC$tot.withinss # SUM OF ALL WITHINSS
KMC$size
healthycluster<-KMC$cluster
healthycluster
dim(healthycluster)<-c(566,646)
image(healthycluster,axes=FALSE,col = rainbow(5))
image(healthymatrix,axes=FALSE,col = grey(seq(0,1,length=256)))
par(mfrow = c(1,2)) # to get images in 2X1 format/c(1,2) is for 1 row, 2 column

KMC2<-kmeans(healthyvector,centers = 2,iter.max = 100)
KMC2$tot.withinss

totwithinss = NULL
for (k in 2:10){
  set.seed(1)
  totwithinss[k-1] = (kmeans(healthyvector, centers =k, iter.max = 1000))$tot.withinss
}

par(mfrow = c(1,1))
NumClusters = seq(2,10,1)
plot(NumClusters,SumWithinss,type = "b")

tumor<- read.csv("https://storage.googleapis.com/dimensionl
ess/Analytics/tumor.csv",header= FALSE)
tumormatrix<-as.matrix(tumor)
tumorvector<-as.vector(tumormatrix)

KMC.kcca = as.kcca(KMC,healthyvector)
KMC.kcca
KMC.kcca@totaldist
KMC.kcca@clusinfo
tumorClusters1<-predict(KMC.kcca,newdata=tumorvector)
table(tumorClusters1)
d<-image(tumormatrix,axes=FALSE,col = rainbow(5))

dim(tumormatrix)

movies<-read.table ("https://storage.googleapis.com/dimensionless/Analytics/u.item.txt",header = FALSE,sep = "|",quote="")
str(movies)
colnames(movies)=c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary","Drama","Fantasy","FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
class(movies)
str(movies)
movies<-unique(movies)
str(movies)
# Quick Question 
table(movies$Comedy)
table(movies$Western)
table(movies$Romance,movies$Drama)
distance<-dist(movies[2:20],method = "euclidean")
str(distance)
summary(distance)
set.seed(50)
spl<-sample.split(movies$Title,SplitRatio = 0.8)
table(spl)

ntrain <-subset(movies,spl==T)
ntest  <-subset(movies,spl==F)
distance3<-dist(ntrain[2:20],method = "euclidean")
cluster_train<-hclust(distance3,method = "ward.D")
cluster_train
plot(cluster_train)
clustergroup<-cutree(cluster_train,k=10)
agg_train<-aggregate(ntrain[2:20],list(clustergroup),mean)
#insall flexclustpackage
hcclust.kcca<-as.kcca(object = cluster_train,k=10,data=ntrain[2:20])
pred1<-predict(hcclust.kcca,newdata=ntest[2:20])
verify<-subset(ntest,pred1==8)
View(verify)

set.seed(50)
KMC_n<-kmeans(ntrain[2:20],centers = x,iter.max = 100)
agg_KMC<-aggregate(x= ntrain[2:20],by =list(KMC_n$cluster),mean)
View(agg_KMC)
hcclust.kcca1<-as.kcca(object = KMC_n,k=10,ntrain[2:20])
predic_n2<-predict(hcclust.kcca1,newdata=ntest[2:20])
verify<-subset(ntest,predic_n2==8)
View(verify)
NumClusters <- seq(2,15,1)

plot(NumClusters,,type="b")
KMC_n$tot.withinss

#principal component analysis


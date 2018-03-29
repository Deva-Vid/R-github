flower=read.csv("https://storage.googleapis.com/dimensionless/Analytics/flower.csv",header=FALSE)
flowerMatrix=as.matrix(flower)
str(flowerMatrix)
flowerVector=as.vector(flowerMatrix)
class(flowerVector)
distance<-dist(flowerVector,method = "euclidean")
str(distance)
clusterIntensity<-hclust(distance,method="ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity,3)
flowerClusters<-cutree(clusterIntensity,k=3)
str(flowerClusters)
tapply(flowerVector,flowerClusters,mean)

dim(flowerClusters)= c(50,50)
flowerClusters
class(flowerClusters)

image(flowerClusters,axes=FALSE)
image(flowerMatrix,axes=FALSE,col = grey(seq(0,1,length=256)))

# MRI Image segmentation
# Let's try this with an MRI image of the brain

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)



# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# K means clustering
# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
healthyClusters<-KMC$cluster

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

#Image 
image(healthyClusters,axes=FALSE,col = rainbow(k))

# Scree Plots

SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
NumClusters = seq(2,10,1)
plot(NumClusters,SumWithinss,type="b")

# Tumor 

tumor<-read.csv("https://storage.googleapis.com/dimensionless/Analytics/tumor.csv",header=FALSE)
str(tumor)
tumorMatrix<-as.matrix(tumor)
tumorVector<-as.vector(tumorMatrix)
image(tumorMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

KMC.kcca = as.kcca(KMC, healthyVector)
KMC.kcca

summary(KMC.kcca)
tumorClusters<-predict(KMC.kcca,newdata=tumorVector)
summary(tumorClusters)
str(tumorClusters)
table(tumorClusters)

# Segmented Image

dim(tumorClusters)<-c(nrow(tumorMatrix),ncol(tumorMatrix))
dim(tumorMatrix)

image(tumorClusters,axes=FALSE,col=rainbow(k))

# imported dailykos.csv file using import dataset

setwd("C:/Users/DiGaNT/Desktop/DEv/Data science/Machine learning")
View(dailykos)
kos <- read.csv("dailykos.csv")
View(kos)
str(kos)
kdist = dist(kos,method = "euclidean")
clustkos = hclust(kdist,method = "ward")
# ploting dendogram of hierarchical model
plot(clustkos)

#hierarchical clustering

clustergroup = cutree(clustkos,k=7)
kos1 = subset(kos,clustergroup == 1)
kos2 = subset(kos,clustergroup == 2)
kos3 = subset(kos,clustergroup == 3)
kos4 = subset(kos,clustergroup == 4)
kos5 = subset(kos,clustergroup == 5)
kos6 = subset(kos,clustergroup == 6)
kos7 = subset(kos,clustergroup == 7)
# Number of observation 
dim(kos1)
dim(kos2)  
dim(kos3)
dim(kos4)
dim(kos5)
dim(kos6)
dim(kos7)
# Number of observation 
table(clustergroup)
tail(sort(colMeans(kos1)))
tail(sort(colMeans(kos2)))
tail(sort(colMeans(kos3)))
tail(sort(colMeans(kos4)))
tail(sort(colMeans(kos5)))
tail(sort(colMeans(kos6)))
tail(sort(colMeans(kos7)))

#Prob 1.6 - 1) Ans 3 , 2) cluster 5 3) kos7

# kmeans clustering

set.seed(1000)
k = 7
KMC2 <- kmeans(kos[-1], centers = k)
dailyKosClusters <- KMC2$cluster
sum(dailyKosClusters==7)
table(KMC2$cluster)



KmeansCluster1 = subset(kos, KMC2$cluster == 1)
KmeansCluster2 = subset(kos, KMC2$cluster == 2)
KmeansCluster3 = subset(kos, KMC2$cluster == 3)
KmeansCluster4 = subset(kos, KMC2$cluster == 4)
KmeansCluster5 = subset(kos, KMC2$cluster == 5)
KmeansCluster6 = subset(kos, KMC2$cluster == 6)
KmeansCluster7 = subset(kos, KMC2$cluster == 7)


tail(sort(colMeans(KmeansCluster1[-1])))
tail(sort(colMeans(KmeansCluster2[-1])))
tail(sort(colMeans(KmeansCluster3[-1])))
tail(sort(colMeans(KmeansCluster4[-1])))
tail(sort(colMeans(KmeansCluster5[-1])))
tail(sort(colMeans(KmeansCluster6[-1])))
tail(sort(colMeans(KmeansCluster7[-1])))
 
# prob 2.4 Ans:- kos2 ,2.5 ans:- Nun , 2.6 ans:- kos7 
dailykos<-read.csv("dailykos.csv")
names(dailykos)
str(dailykos)
head(dailykos)
distance<-dist(dailykos,method = "euclidean")
clustDaily<-hclust(distance,method = "ward.D")
plot(clustDaily)
clusterData<-cutree(clustDaily,7)
str(clusterData)
summary(clusterData)
agg<-aggregate(dailykos,list(clusterData),mean)
tail(sort(agg[1,]))
clust1<-subset(dailykos,clusterData==1)

clust3<-subset(dailykos,clusterData==3)
nrow(clust3)
HierCluster = split(dailykos, clusterData)
str(HierCluster)
nrow(HierCluster[[3]])
lapply(HierCluster,nrow)
sapply(1:7,function(x){nrow(HierCluster[[x]])})
for (i in 1:7) {
  clust[[i]]<-subset(dailykos,clusterData==i)
  print (nrow(clust[[i]]))
}
tail(sort(colMeans(HierCluster[[1]])))
for (i in 1:7){
  print (tail(sort(colMeans(HierCluster[[i]]))))}









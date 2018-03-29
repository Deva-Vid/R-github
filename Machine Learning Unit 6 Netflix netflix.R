movies<-read.table ("https://storage.googleapis.com/dimensionless/Analytics/u.item.txt",header = FALSE,sep = "|",quote="")
 str(movies)
 head(movies)
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
clusterMovies<-hclust(d = distance,method="ward.D")
plot(clusterMovies)
clusterGroups<-cutree(clusterMovies,k=10)
plot(clusterGroups)
table(clusterGroups,movies$Action)
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
colMeans(subset(movies[2:20], clusterGroups == 1))

spl = split(movies[2:20], clusterGroups)
spl[[1]]
lapply(spl, colMeans)


# Recommendation Systems
subset(movies,Title=="Men in Black (1997)")
clusterGroups[257]
movies2<- subset(movies,clusterGroups==2)
movies2$Title


# Quick Question 
clusterGroups2<-cutree(clusterMovies,k=2)
colMeans(subset(movies[2:20], clusterGroups2 == 2))


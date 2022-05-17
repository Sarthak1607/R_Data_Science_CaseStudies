setwd("C:/Users/Souvik/Downloads/PPA")

library(cluster)
library(factoextra)

### K-Means Clustering ###

USArrest <- read.csv("USArrest.csv", row.names = 1)
View(USArrest)
summary(USArrest)

USArrest1 <- scale(USArrest)
View(USArrest1)

km <- kmeans(USArrest1,2)
fviz_cluster(km, data=USArrest1)
str(km)

Accuracy <- km$betweenss/km$totss
Accuracy

##### ELBOW METHOD #####

number <- 1:10
wss <- 1:10

for (i in 1:10)
{
  wss[i] <- kmeans(USArrest1,i)$tot.withinss
}
wss
plot(number,wss,type = "b", pch=19)

###Optimal number of cluster 
km1 <- kmeans(USArrest1,4)
fviz_cluster(km1, data=USArrest1)
str(km1)

Accuracy <- km1$betweenss/km1$totss
Accuracy

## Save Cluster in Original dataset ##

USArrest$cluster <- km1$cluster
View(USArrest)

### Profiling of Clusters ####

cmeans <- aggregate(USArrest, by=list(USArrest$cluster),mean)
cmeans




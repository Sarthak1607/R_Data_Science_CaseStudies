setwd("C:/Users/Souvik/Downloads/PPA")

library(cluster)
library(factoextra)

SECB <- read.csv("PPA SEC B 2020.csv", row.names = 1, stringsAsFactors = TRUE)
summary(SECB)
table(SECB$Personality.Trait)

### Preparing the dataset for clustering
SECB$Drink<- ifelse(SECB$Drink == "Yes", 1,0)
SECB$Food.Preference <- ifelse(SECB$Food.Preference == "Non Veg", 1,0)
SECB$Personality.Trait_A <- ifelse(SECB$Personality.Trait == "Agreeableness", 1,0)
SECB$Personality.Trait_C <- ifelse(SECB$Personality.Trait == "Conscientiousness", 1,0)
SECB$Personality.Trait_E <- ifelse(SECB$Personality.Trait == "Extraversion", 1,0)
SECB$Personality.Trait_N <- ifelse(SECB$Personality.Trait == "Neuroticism", 1,0)
SECB$Personality.Trait_O <- ifelse(SECB$Personality.Trait == "Openness", 1,0)
SECB$Grade=ifelse(SECB$Grade=='A+',1,(ifelse(SECB$Grade=='A',2,3)))

## Creating a new dataset removing one column ###
SECB_PPA = subset(SECB, select = -c(Personality.Trait))


### k-means clustering ####
##### ELBOW METHOD #####

number <- 1:10
wss <- 1:10

for (i in 1:10)
{
  wss[i] <- kmeans(SECB_PPA,i)$tot.withinss
}
wss
plot(number,wss,type = "b", pch=19)

###Taking Optimal number of cluster = 4 
#judging from the from the elbow method####  
km <- kmeans(SECB_PPA,4)
fviz_cluster(km, data=SECB_PPA)
str(km)

Accuracy <- km$betweenss/km$totss
Accuracy

## Save Cluster in Original dataset ##

SECB_PPA$cluster <- km$cluster

### Profiling of Clusters ####

cmeans <- aggregate(SECB_PPA, by=list(SECB_PPA$cluster),mean)
cmeans


#### Hierarchical Clustering ###

dmatrix <- daisy(SECB_PPA, metric = c("euclidean"), stand = TRUE)
class(dmatrix)
dmatrix1 <- dist(dmatrix)
fviz_dist(dmatrix1, lab_size = 8)

d <- as.matrix(dmatrix1)
write.csv(d, "D_MATRIX.csv")

hc <- hclust(dmatrix,method = "average")
plot(as.dendrogram(hc))

cluster <- rect.hclust(hc,4)
cluster


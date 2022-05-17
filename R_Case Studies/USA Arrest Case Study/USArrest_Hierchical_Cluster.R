setwd("C:/Users/Souvik/Downloads/PPA")

library(cluster)
library(factoextra)

USArrest <- read.csv("USArrest.csv", row.names = 1)
View(USArrest)
summary(USArrest)

## Calculate distnce matrix ###

dmatrix <- daisy(USArrest, metric = c("euclidean"), stand = TRUE)
class(dmatrix)
dmatrix1 <- dist(dmatrix)
fviz_dist(dmatrix1, lab_size = 8)

d <- as.matrix(dmatrix1)
View(d)
write.csv(d, "D_MATRIX.csv")

#### Hierchical Clustering ####

hc <- hclust(dmatrix,method = "average")
plot(as.dendrogram(hc))

cluster <- rect.hclust(hc,5)
cluster


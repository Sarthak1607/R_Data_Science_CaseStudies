setwd("C:/Users/Souvik/Downloads/PPA")

library(rpart)
library(rattle)
library(rpart.plot)
library(e1071)
library(caTools)
library(caret)

heart <- read.csv("heart.csv")
View(heart)

heart$target <- as.factor(heart$target)
summary(heart)

split <- sample.split(heart$target, SplitRatio = 0.7)
heartTR <- subset(heart, split == "TRUE")
heartTS <- subset(heart, split == "FALSE")

model <- rpart(target ~ ., data = heartTR, 
               method = "class", 
               parms = list(split = "gini"))
model

fancyRpartPlot(model)

pred <- predict(model, newdata = heartTS, type = "class")
confusionMatrix(pred,heartTS$target)





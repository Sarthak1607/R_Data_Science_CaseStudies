setwd("C:/Users/Souvik/Downloads/PPA")

library(car)
library(corrplot)
library(caret)
library(caTools)
library(psych)

insurance <- read.csv("insurance_LR1.csv", stringsAsFactors = TRUE)
dim(insurance)
str(insurance)
summary(insurance)

#to convert variable into factor variable
insurance$sex <- as.factor(insurance$sex)
summary(insurance)

insurance$children <- as.factor(insurance$children)
summary(insurance)

#treat Missing values
#is.na(insurance$bmi)

#insurance$bmi[is.na(insurance$bmi)] <- mean(insurance$bmi, na.rm = TRUE)
#insurance$bmi[is.na(insurance$bmi)] <- 0
insurance <- na.omit(insurance)

cr <- cor(insurance[c("age","bmi","charges")])
cr
corrplot(cr, type = "full")
corrplot(cr,method = "number")
corrplot.mixed(cr)

#To Dummy variables

insurance$smoker_y <- ifelse(insurance$smoker == "yes", 1,0)
insurance$smoker_n <- ifelse(insurance$smoker == "no", 1,0)
insurance$region_se <- ifelse(insurance$region == "southeast", 1,0)
insurance$region_ne <- ifelse(insurance$region == "northeast", 1,0)
insurance$region_sw <- ifelse(insurance$region == "southwest", 1,0)
insurance$region_nw <- ifelse(insurance$region == "northwest", 1,0)
View(insurance)
#Splitting of dataset into training and testing

split <- sample.split(insurance$charges, SplitRatio = 0.7)
training_data <- subset(insurance, split == "TRUE")
testing_data <- subset(insurance, split == "FALSE")

#pair.panels

pairs.panels(training_data[c("age","bmi","charges")])

#Linear Regression
model1 <- lm(charges ~ age, data = training_data)
summary(model1)

model2 <- lm(charges ~ age+bmi, data = training_data)
summary(model2)

model3 <- lm(charges ~ age+bmi+smoker_y, data = training_data)
summary(model3)

model4 <- lm(charges ~ age+bmi+smoker_y+region_nw, data = training_data)
summary(model4)

model5 <- lm(charges ~ age+bmi+smoker_y, data = training_data)
summary(model5)

plot(model5$fitted.values,model5$residuals)

#prediction 
prediction <- predict(model5, testing_data) 
head(prediction)
head(testing_data$charges)

plot(testing_data$charges,type="l",col="green")
lines(prediction,type="l",col="blue")

#marketing dataset
#amount spend dependent variable


#set WD on top
#Write all the libraries on the top
#There should be no views in your code
#No error in model



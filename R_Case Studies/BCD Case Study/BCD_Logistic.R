setwd("C:/Users/Souvik/Downloads/PPA")

library(corrplot)
library(car)
library(ROCR)

BCD <- read.csv("BCD.csv")
View(BCD)
summary(BCD)
BCD <- na.omit(BCD)
summary(BCD)

BCD$class <- ifelse(BCD$class == "malignant",1,0)
cr <- cor(BCD)
cr
corrplot(cr,type = "full")
corrplot.mixed(cr)

BCD$class <- as.factor(BCD$class)

#Logistic regression 

model0 <- glm(class ~ thick, data=BCD, family = binomial)
summary(model0)

#adding variables 

model1 <- glm(class ~ thick+u.size, data=BCD, family = binomial)
summary(model1)

model2 <- glm(class ~ thick+u.size+u.shape, data=BCD, family = binomial)
summary(model2)

model3 <- glm(class ~ thick+u.size+u.shape+adhsn, data=BCD, family = binomial)
summary(model3)

model4 <- glm(class ~ thick+u.size+u.shape+adhsn+s.size, data=BCD, family = binomial)
summary(model4)

model5 <- glm(class ~ thick+u.size+u.shape+adhsn+s.size+nucl, data=BCD, family = binomial)
summary(model5)

model6 <- glm(class ~ thick+u.size+u.shape+adhsn+s.size+nucl+chrom, data=BCD, family = binomial)
summary(model6)

model7 <- glm(class ~ thick+u.size+u.shape+adhsn+s.size+nucl+chrom+n.nuc, data=BCD, family = binomial)
summary(model7)

model8 <- glm(class ~ thick+u.size+u.shape+adhsn+s.size+nucl+chrom+n.nuc+mit, data=BCD, family = binomial)
summary(model8)

#removing variables

model9 <- glm(class ~ thick+u.shape+adhsn+s.size+nucl+chrom+n.nuc+mit, data=BCD, family = binomial)
summary(model9)

model10 <- glm(class ~ thick+u.shape+adhsn+nucl+chrom+n.nuc+mit, data=BCD, family = binomial)
summary(model10)

model11 <- glm(class ~ thick+u.shape+adhsn+nucl+chrom+n.nuc, data=BCD, family = binomial)
summary(model11)

#We will take model 10 , Residual least AIC least 

#prediction and confusion matrix

res <- predict(model10,BCD, type='response')
head(res)
head(BCD$class)

table(Actualvalue=BCD$class, PredictedValue= res>0.2)

#ROCR Curve 

ROCRpred <- prediction(res,BCD$class)
ROCRpref <- performance(ROCRpred, "tpr","fpr")
plot(ROCRpref,colorize = TRUE, print.cutoffs.at=seq(0.2,by=0.3))


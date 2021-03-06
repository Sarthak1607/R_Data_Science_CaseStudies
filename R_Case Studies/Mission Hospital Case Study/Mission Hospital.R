setwd("C:/Users/Souvik/Downloads/PPA")

library(car)
library(corrplot)
library(caret)
library(caTools)
library(psych)

Mission_Hospital <- read.csv("Mission Hospital.csv", stringsAsFactors = TRUE)
View(Mission_Hospital)
dim(Mission_Hospital)
str(Mission_Hospital)
summary(Mission_Hospital)
which(is.na(Mission_Hospital))
which(is.na(Mission_Hospital$BODY_WEIGHT))

#Simple Linear Regression to check if there is association b/w  Total Cost and Body Weight 
model0 <- lm(TOTAL_COST_TO_HOSPITAL  ~ BODY_WEIGHT, data = Mission_Hospital)
summary(model0)

#the correlation between variable "Age", "Body Weight", "Body Height", 
#"Total Length of Stay", "Length of Stay ICU", "Cost of Implant", "Total Cost to Hospital"

which(is.na(Mission_Hospital$AGE))
which(is.na(Mission_Hospital$BODY_HEIGHT))
which(is.na(Mission_Hospital$TOTAL_LENGTH_OF_STAY))
which(is.na(Mission_Hospital$LENGTH_OF_STAY_ICU))
which(is.na(Mission_Hospital$COST_OF_IMPLANT))
which(is.na(Mission_Hospital$TOTAL_COST_TO_HOSPITAL))

cr <- cor(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                             "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])
cr
corrplot(cr, type = "full")
corrplot(cr,method = "number")
corrplot.mixed(cr)

#pairs panels 
pairs.panels(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                                "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])

#log transformation
Mission_Hospital$TOTAL_COST_TO_HOSPITAL <- log(Mission_Hospital$TOTAL_COST_TO_HOSPITAL)
View(Mission_Hospital$TOTAL_COST_TO_HOSPITAL)
cr <- cor(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                             "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])
cr
corrplot(cr, type = "full")
corrplot(cr,method = "number")
corrplot.mixed(cr)
pairs.panels(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                                "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])


#Models Creating

model1 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE, data = Mission_Hospital)
summary(model1)

model2 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+BODY_WEIGHT , data = Mission_Hospital)
summary(model2)

model3 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+BODY_HEIGHT , data = Mission_Hospital)
summary(model3)

model4 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY , data = Mission_Hospital)
summary(model4)

model5 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU , data = Mission_Hospital)
summary(model5)

model6 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT , data = Mission_Hospital)
summary(model6)

model7 <- lm(TOTAL_COST_TO_HOSPITAL  ~ BODY_WEIGHT+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT , data = Mission_Hospital)
summary(model7)

model8 <- lm(TOTAL_COST_TO_HOSPITAL  ~ BODY_HEIGHT+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT , data = Mission_Hospital)
summary(model8)

#prediction 
prediction <- predict(model8, Mission_Hospital) 
head(prediction)
#converting log transformation to exponential
prediction <- exp(prediction)
head(prediction)

ML <- exp(Mission_Hospital$TOTAL_COST_TO_HOSPITAL)
head(ML)

plot(ML,type="l",col="green")
lines(prediction,type="l",col="blue")


#Heteroscedasticity check 
plot(model7$fitted.values,model7$residuals)
plot(model8$fitted.values,model8$residuals)





setwd("C:/Users/Souvik/Downloads/PPA")

library(arules)
library(arulesViz)

grocery <- read.transactions("Grocery.csv", sep=",")
summary(grocery)

itemFrequencyPlot(grocery, topN=100)

rules <- apriori(grocery, parameter = list(supp=0.01, conf=0.0))
inspect(rules)
write(rules,file = "grocery_rules_1.csv", sep=",")

plot(rules,method="grouped")
plot(rules, method="graph",control = list(type="item"),
                                          interactive= T)


     
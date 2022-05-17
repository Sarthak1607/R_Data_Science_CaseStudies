setwd("C:/Users/Souvik/Downloads/PPA")

library(arules)
library(arulesViz)
library(dplyr)

BigBasket <- read.csv("bigbasket.csv")
View(BigBasket)

big <- BigBasket %>% group_by(Order) %>%
  summarise(Items = paste(Description,collapse = ","))
View(big)

write.csv(big$Items,"big1.csv",quote = FALSE, row.names = FALSE)

big1 <- read.transactions("big1.csv", sep=",")
summary(big1)

itemFrequencyPlot(big1, topN=100)

rules <- apriori(big1, parameter = list(supp=0.01, conf=0.0))
inspect(rules)
write(rules,file = "bigbasket_rules_1.csv", sep=",")

plot(rules,method="grouped")
plot(rules, method="graph",control = list(type="item"),
     interactive= T)


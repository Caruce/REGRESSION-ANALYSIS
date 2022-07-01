Sales.Data <- read.csv("COMPPURCH10.csv")

head(Sales.Data)
attach(Sales.Data)
plot(AGE, INCOME, pch=16+PURCHASE, col=1+PURCHASE)

Logit.fit=glm(PURCHASE~AGE + INCOME, family = "binomial")

summary(Logit.fit)

#According to summary output, both income and age are important factors in purchasing decision

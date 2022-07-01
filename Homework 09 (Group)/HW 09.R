COMPPURCH10 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 09 (Group)/COMPPURCH10.csv")

summary(COMPPURCH10)

plot(COMPPURCH10$AGE,COMPPURCH10$INCOME, pch=16+COMPPURCH10$PURCHASE,col= 1+COMPPURCH10$PURCHASE)

performanceModel <- glm(PURCHASE~AGE+INCOME, data = COMPPURCH10, family = "binomial")
summary(performanceModel)

z_criticial <- qnorm(0.975)

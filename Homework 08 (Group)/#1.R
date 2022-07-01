BC <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 08 (Group)/BOOKCOST7.csv")

plot(BC$COST,BC$PAGES, pch=16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)

lm1 <- lm(COST~PAGES+SOFTCOVER, data = BC)

summary(lm1)

install.packages("alr3")
library("alr3")

pure.error.anova(lm1)

qf(0.95, df1 = 86, df2 = 118)


plot(lm1, pch=16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)

install.packages("lmtest")
library("lmtest")
dwtest(lm1)


install.packages("car")
library("car")
ncvTest(lm1)

lm2 <- lm(COST~PAGES, data = BC)
plot(lm2,pch = 16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)

plot(COST~PAGES, pch = 16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER, data = BC)

BC$COST_sqrt <- sqrt(BC$COST)
cost.lm1 <- lm(COST_sqrt~PAGES, data = BC)
plot(cost.lm1,pch = 16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)
ncvTest(cost.lm1)

BC$COST_log <- log(BC$COST)
cost.lm2 <- lm(COST_log~PAGES, data = BC)
plot(cost.lm2,pch = 16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)
ncvTest(cost.lm2)


install.packages("nortest")
library("nortest")
lm3 <- lm(COST~PAGES+SOFTCOVER, data=BC)
ad.test(lm3$residuals)
plot(lm3, pch = 16+BC$SOFTCOVER,col= 1+BC$SOFTCOVER)

ad.test(lm1$residuals)

lm4 <- lm(COST~PAGES+SOFTCOVER+PAGES*SOFTCOVER, data=BC)
summary(lm4)
library("HH")
vif(lm4)

library("alr3")
pureErrorAnova(lm4)
qf(0.95, df1 = 85, df2 = 118)

ncvTest(lm4)


BC$P.SC = BC$PAGES*BC$SOFTCOVER 
lm5 <- lm(COST~PAGES+P.SC, data=BC)
vif(lm5)
summary(lm5)
pureErrorAnova(lm5)
qf(0.95, df1 = 86, df2 = 118)

ncvTest(lm5)

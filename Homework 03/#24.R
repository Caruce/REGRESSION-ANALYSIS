t_critical <- qt(1-0.05/2,df=24-2)

Costest3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 4/Homework 3/Costest3.csv")

head(Costest3)
attach(Costest3)

com.lm <- lm(COST~NUMBER)
summary(com.lm)
predict(com.lm, data.frame(ports = 25), se.fit = TRUE, interval = "confidence", level = 0.95)


mean_number = mean(Costest3$NUMBER)
var_Sx = var(Costest3$NUMBER)

anova(com.lm)

2.84 * 2.84

sqrt(0.09998436)
sqrt(0.07080633)

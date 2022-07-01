Incons3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 4/Homework 3/Incons3.csv")


attach(Incons3)

com.lm <- lm(CONS~INCOME)
summary(com.lm)




t_critical_25 <- qt(1-0.05/2,df=12-2)
mean_25 = mean(Incons3$INCOME)

var_25 = var(Incons3$INCOME)

t_critical_25_2 <- qt(1-0.1/2,df=12-2)

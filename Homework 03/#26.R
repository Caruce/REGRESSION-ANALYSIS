Apex3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 4/Homework 3/Apex3.csv")

attach(Apex3)

com.lm <- lm(COST~MACHINE)
summary(com.lm)


t_critical_26 <- qt(1-0.05/2,df=27-2)

mean_26 = mean(Apex3$MACHINE)
var_26 = var(Apex3$MACHINE)

sqrt(0.059359)

t_critical_26_2 <- qt(1-0.01/2,df=27-2)

sqrt(0.70334)

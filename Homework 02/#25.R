Incons3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 3/HW 2/Incons3.csv")
head(Incons3)

Inc.lm <- lm(CONS~INCOME, data = Incons3)
summary(Inc.lm)

plot(Incons3$INCOME~Incons3$CONS, pch = 20)
abline(Inc.lm, col = "yellow", lwd = 4)

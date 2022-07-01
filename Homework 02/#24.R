Costest3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 3/HW 2/Costest3.csv")
head(Costest3)

cost.lm <- lm(COST~NUMBER, data = Costest3)
summary(cost.lm)

plot(Costest3$NUMBER, Costest3$COST, pch=19)

abline(cost.lm, col = "red", lwd = 4)

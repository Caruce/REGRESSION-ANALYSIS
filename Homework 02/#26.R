Apex3 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/week 3/HW 2/Apex3.csv")
head(Apex3)

apex.lm <- lm(COST~MACHINE, data = Apex3)
summary(apex.lm)

plot(Apex3$MACHINE, Apex3$COST, pch=20)
abline(apex.lm, col = "green", lwd = 4)

COMPREP5 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 05 (Group)/COMPREP5.csv")
lm1 <- lm(TIME ~ EXPER + NUMBER, data = COMPREP5)
plot(lm1, pch = 16)
summary(lm1)

plot(COMPREP5[c(1,2,3)],pch = 16)

COMPREP5$NUMBER_square=COMPREP5$NUMBER*COMPREP5$NUMBER
lm2 <- lm(TIME ~ EXPER + NUMBER + NUMBER_square, data = COMPREP5)
summary(lm2)

predict(lm2, data.frame(EXPER = 4, NUMBER = 3, NUMBER_square = 9), interval = "predict")

predict(lm1, data.frame(EXPER = 5, NUMBER = 10), interval = "predict")

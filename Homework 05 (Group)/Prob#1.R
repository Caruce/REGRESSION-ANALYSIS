NBA4 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 05 (Group)/NBA4.csv")

lm1 <- lm(WINS ~ FGM + TFGA + TFGM + OFFREB, data = NBA4)
summary(lm1)
plot(NBA4[c(2,4,7,8,11)],pch = 16)

cor(NBA4[c(2,4,7,8,11)])

install.packages("HH")
require("HH")
vif(NBA4[c(2,4,7,8,11)])

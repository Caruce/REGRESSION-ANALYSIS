MLB4 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 04 (Group)/MLB4.csv")

Mlb_2 <- lm(WINS~BA+ERA,data = MLB4)
summary(Mlb_2)

Mlb_3 <- lm(WINS~BA+ERA+HR,data = MLB4)
summary(Mlb_3)

predict(Mlb_3, data.frame(BA = 0.249, ERA = 4.91, HR = 192), interval = "predict",level = 0.95)

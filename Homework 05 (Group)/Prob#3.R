CRIMSPN5 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 05 (Group)/CRIMSPN5.csv")


CRIMSPN5$EXPEND <- CRIMSPN5$EXPEND/1000000
plot(CRIMSPN5[c(3,2)],pch = 16)

CRIMSPN5$POLICE_square = CRIMSPN5$POLICE*CRIMSPN5$POLICE
lm1<- lm(EXPEND ~ POLICE + POLICE_square, data = CRIMSPN5)
summary(lm1)

lm2<- lm(EXPEND ~ POLICE , data = CRIMSPN5)
summary(lm2)

CRIMSPN5$POLICE_sqrt <- sqrt(CRIMSPN5$POLICE)
plot(CRIMSPN5$POLICE_sqrt, CRIMSPN5$EXPEND, pch = 16) 
lm3<- lm(EXPEND ~ POLICE_sqrt, data = CRIMSPN5)
summary(lm3)


CRIMSPN5$POLICE_log <- log(CRIMSPN5$POLICE)
plot(CRIMSPN5$POLICE_log, CRIMSPN5$EXPEND, pch = 16) 
lm4<- lm(EXPEND ~ POLICE_log, data = CRIMSPN5)
summary(lm4)


CRIMSPN5$POLICE_rec <- 1/CRIMSPN5$POLICE
plot(CRIMSPN5$POLICE_rec, CRIMSPN5$EXPEND, pch = 16) 
lm5<- lm(EXPEND ~ POLICE_rec, data = CRIMSPN5)
summary(lm5)


CRIMSPN5$POLICE_rec2 <- 1/(CRIMSPN5$POLICE)^2
plot(CRIMSPN5$POLICE_rec2, CRIMSPN5$EXPEND, pch = 16) 
lm6<- lm(EXPEND ~ POLICE_rec2, data = CRIMSPN5)
summary(lm6)


predict(lm2, data.frame(POLICE = 10000), interval = "confidence")

FUELCON4 <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 04 (Group)/FUELCON4.csv")
head(FUELCON4)

Fuelcon_2 <- lm(FUELCON~GASTAX+INCOME, data = FUELCON4)
summary(Fuelcon_2)

is.numeric(FUELCON4$HWYMILES)
FUELCON4$HWYMILES <- gsub(",","",FUELCON4$HWYMILES)
FUELCON4$HWYMILES <- as.numeric(FUELCON4$HWYMILES)



Fuelcon_4 <- lm(FUELCON~GASTAX+INCOME+DRIVERS+HWYMILES, data = FUELCON4)
summary(Fuelcon_4)

qf(.95,df1 = 4,df2 = 46)


Fuelcon_3 <- lm(FUELCON~GASTAX+INCOME+DRIVERS, data = FUELCON4)

predict(Fuelcon_3, data.frame(DRIVERS = 0.74, GASTAX = 22, INCOME = 28619), interval = "confidence",level = 0.95)

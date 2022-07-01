BBALL6 <- read.csv("C:/Users/GR/Downloads/BBALL6.csv")
plot(BBALL6)


n <- nrow(BBALL6)
BBALL6$ID <- c(1:n)
               
lm1 <- lm(WINS~AVESAL, data = BBALL6)
plot(lm1, pch = 16)

rstandard(lm1)

std.res <- data.frame(ID = c(1:n), stdRes = rstandard(lm1))
tcritical <- qt(0.975, n-1-1)
std.res.outliers <-std.res[abs(std.res$stdRes)>2.04,]

plot(std.res,pch = 16, type="b")
abline(h = 2.04)
abline(h = -2.04)
points(std.res.outliers, pch = 16, col = "red")

text(std.res.outliers, labels = std.res.outliers$ID, pos = 2.04)

rstudent(lm1)
del.res <- data.frame(ID = c(1:n), delRes = rstudent(lm1))

del.res.outliers <- del.res[abs(del.res$delRes) > tcritical,]
del.res.outliers



hatvalues(lm1)
criticalLeverage <- 2*(1+1)/n

leverage <- data.frame(ID = c(1:n), hatv = hatvalues(lm1))
highLev <- leverage[leverage$hatv>criticalLeverage,]
highLev


dffits(lm1)
DFITS <- data.frame(ID = c(1:n), Dfits = dffits(lm1))
influencelimit = 2*sqrt((1+1)/n)
influentialCase <- DFITS[DFITS$Dfits>influencelimit,]
influentialCase

cooks.distance(lm1)
CookD <- data.frame(ID = c(1:n), CD = cooks.distance(lm1))
CDLimit <- qf(0.5,1+1,n-1-1)
CDoutliers <- CookD[CookD$CD>CDLimit,]
CDoutliers

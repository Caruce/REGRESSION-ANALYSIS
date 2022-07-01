LQ <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-fall/STAT/Homework 08 (Group)/LaQuinta.csv")

install.packages("leaps")
library("leaps")

attach(LQ)
VarSel.Cp  <- leaps(x=cbind(ROOMS,NEAREST,OFFICE,COLLEGE,INCOME,DISTTWN), MARGIN, method=c("Cp"), 
                    names = c("ROOMS","NEAREST","OFFICE","COLLEGE","INCOME","DISTTWN"), nbest = 3)
VarSel.Cp.Table <- cbind(VarSel.Cp$which, VarSel.Cp$size, VarSel.Cp$Cp)

VarSel.Cp.Table 

n <- length(VarSel.Cp$size)
dimnames(VarSel.Cp.Table) <- list(1:n,c("ROOMS","NEAREST","OFFICE","COLLEGE","INCOME","DISTTWN","Size","Cp"))

round(VarSel.Cp.Table ,digits=3)
plot(VarSel.Cp$size, VarSel.Cp$Cp)



lm1.1 <- lm(MARGIN~ROOMS+NEAREST+OFFICE+COLLEGE+INCOME+DISTTWN, data = LQ)
summary(lm1.1)

lm1.1 <- lm(MARGIN~ROOMS+NEAREST+OFFICE+COLLEGE+INCOME+DISTTWN, data = LQ)
summary(lm1.1)

lm1.2 <- lm(MARGIN~ROOMS+NEAREST+OFFICE+INCOME+DISTTWN, data = LQ)
summary(lm1.2)

lm1.3 <- lm(MARGIN~ROOMS+NEAREST+OFFICE+INCOME, data = LQ)
summary(lm1.3)


install.packages("MASS")
library("MASS")

lm.Mean <- lm(MARGIN~1, data = LQ)
summary(lm.Mean)
lm.full <- lm(MARGIN~ROOMS+NEAREST+OFFICE+COLLEGE+INCOME+DISTTWN, data = LQ)
summary(lm.full)
addterm(lm.Mean,lm.full, test= "F")

lm2.1 <- update(lm.Mean,.~.+OFFICE)
addterm(lm2.1,lm.full, test= "F")

lm2.2 <- update(lm2.1,.~.+ROOMS)
addterm(lm2.2,lm.full, test= "F")

lm2.3 <- update(lm2.2,.~.+INCOME)
addterm(lm2.3,lm.full, test= "F")

lm2.4 <- update(lm2.3,.~.+NEAREST)
addterm(lm2.4,lm.full, test= "F")

lm2.5 <- update(lm2.4,.~.+DISTTWN)
addterm(lm2.5,lm.full, test= "F")


addterm(lm.Mean,lm.full, test= "F")
lm3.1 <- update(lm.Mean,.~.+OFFICE)
dropterm(lm3.1, test="F")

addterm(lm3.1,lm.full, test= "F")
lm3.2 <- update(lm3.1,.~.+ROOMS)
dropterm(lm3.2, test="F")

addterm(lm3.2,lm.full, test= "F")
lm3.3 <- update(lm3.2,.~.+INCOME)
dropterm(lm3.3, test="F")

addterm(lm3.3,lm.full, test= "F")
lm3.4 <- update(lm3.3,.~.+NEAREST)
dropterm(lm3.4, test="F")

addterm(lm3.4,lm.full, test= "F")
lm3.5 <- update(lm3.4,.~.+DISTTWN)
dropterm(lm3.5, test="F")

addterm(lm3.5,lm.full, test= "F")
lm3.6 <- update(lm3.5,.~.+COLLEGE)
dropterm(lm3.6, test="F")


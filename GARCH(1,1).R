#3.3-R
library(fGarch)
sp5=scan(file = "sp500.txt")
plot(sp5,type="l")
#Below,fit an AR(3)+GARCH(1,1) mmodel
m1<-garchFit(~arma(3,0)+garch(1,1),data=sp5,trace=F)
summary(m1)
#Below,fit a GARCH(1,1) model with Student-t distribution
m2<-garchFit(~garch(1,1),data =sp5,trace = F,cond.dist = "std" )
summary(m2)
#obtain standardized residuals
stresi<-residuals(m2,standardize=T)
plot(stresi,type="l")
Box.test(stresi,10,type="Ljung")
predict(m2,5)

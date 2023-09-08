rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)

getSymbols("MRTSSM44X72USN",src = "FRED")
MR <- zoo(MRTSSM44X72USN$MRTSSM44X72USN, index(MRTSSM44X72USN))
Date <- as.yearmon(index(MR), "%Y %m")

Y <- log(MR)
T <- length(Y)
t <- 1:T
M <- factor(cycle(Date))  
omega <- c(2*pi/6,2*pi/12)
cosTerm1 <- cos(omega[1]*t)
cosTerm2 <- cos(omega[2]*t)
sinTerm1 <- sin(omega[1]*t)
sinTerm2 <- sin(omega[2]*t)



m1 <- lm(Y ~ t + relevel(M, ref = 6))
m2 <- lm(Y ~ t + I(t^2) + relevel(M, ref = 6))
m3 <- lm(Y ~ t + I(t^2) + I(t^3) + relevel(M, ref = 6))
m4 <- lm(Y ~ t + cosTerm1 + sinTerm1 + cosTerm2 + sinTerm2)
m5 <- lm(Y ~ t + I(t^2) + cosTerm1 + sinTerm1 + cosTerm2 + sinTerm2)
m6 <- lm(Y ~ t + I(t^2) + I(t^3) + cosTerm1 + sinTerm1 + cosTerm2 + sinTerm2)


sig2 <- c(sum(m1$residuals^2)/T,sum(m2$residuals^2)/T,sum(m3$residuals^2)/T,
          sum(m4$residuals^2)/T, sum(m5$residuals^2)/T, sum(m6$residuals^2)/T)


K <- c(12, 13, 14, 5, 6, 7)


AIC <- log(sig2) + (T+2*K)/T

AICc <- log(sig2) + (T+K)/(T-K-2)

BIC <- log(sig2) + K*log(T)/T


rbind(AIC,AICc,BIC)

par(mfrow = c(4,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(Y, type = "n",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(index(Y),Y,lwd = 2,type = "o")
lines(m3$fitted.values, col = "steelblue", lwd = 2)
lines(index(Y),m3$coefficients[1] + m3$coefficients[2]*t +m3$coefficients[3]*t^2+m3$coefficients[4]*t^3,lwd = 2,col = "indianred")
legend("top",c("Log Sales","Fitted values","Trend"), col=c("black","steelblue", "indianred"),lty = 1,lwd = 2, cex=1,bty = "n")
plot(index(Y),m3$residuals, type = "n",ylab = "Residuals",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(m3$residuals,lwd = 2, col = "steelblue")
acf(as.numeric(m3$residuals))
qqnorm(as.numeric(m3$residuals))
qqline(as.numeric(m3$residuals), col = "steelblue")


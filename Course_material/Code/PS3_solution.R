rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)
library(forecast)

setwd("C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2022")
savehere <- paste0(getwd(),'/Plots/')
source("Code/Kalman.R")

getSymbols("HOUSTNSA",src="FRED") #Requires quantmod to be loaded
HOUSE <- zoo((HOUSTNSA$HOUSTNSA), as.yearmon(index(HOUSTNSA)))

par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(HOUSE,xlab = "Year",ylab = "Log Housing Starts", type = "n")
grid(lty=1, col = gray(.9))
lines(HOUSE,lwd = 2)

acf2(HOUSE)


T <- length(HOUSE)
t <- 1:T

M <- factor(cycle(HOUSE))

summary(trend <- lm(HOUSE ~ t  +   M ))

plot(HOUSE, col = "steelblue")
lines(trend$fitted.values, col = "indianred")


# maxp <- 3
# maxq <- 3
# AIC <- matrix(NA,maxp+1,maxq+1)
# BIC <- matrix(NA,maxp+1,maxq+1)
# 
# for (p in 0:maxp){
#   for(q in 0:maxq){
#     model <- arima(x=as.numeric(trend$residuals), order=c(p,0,q), method="CSS-ML")
#     AIC[p+1,q+1] <- AIC(model); BIC[p+1,q+1] <- BIC(model)
#   }
# }
# AIC # ARMA(3,3) is preferred
# BIC # ARMA(1,1) is preferred

model <- arima(x=as.numeric(trend$residuals), order=c(1,0,1), method="CSS-ML") #Estimate preferred model


## SS approach

useData <- HOUSE

y <- t(as.numeric(HOUSE))
T <- ncol(y) 
A <- cbind(1,0,1,0,0,0,0,0,0,0,0,0,0)
mu0 <- as.matrix(rep(0,13)); 
Sigma0 <- diag(10^9,13)
Phi <- rbind(c(1,1,rep(0,11)),c(0,1,rep(0,11)),c(0,0,rep(-1,11)),cbind(rep(0,10),rep(0,10),diag(1,10),rep(0,10)))
Theta <- cbind(c(1,rep(0,12)),c(0,1,rep(0,11)),c(0,0,1,rep(0,10)))

func <- function(par){
  kf <- KFilter(y,A,mu0,Sigma0,Phi,Theta,as.matrix(exp(par[1])),diag(exp(c(par[2],par[3],par[4]))))
  return(-kf$like) 
}

est <- optim(c(log(sqrt(var(HOUSE)))/2, log(sqrt(var(HOUSE)))/2, 1, 0), func, gr=NULL, method='BFGS', hessian=FALSE,
             control=list(trace=1, REPORT=1,maxit = 50000))

exp(est$par*2)

kf <- KSmooth(y,A,mu0,Sigma0,Phi,Theta,as.matrix(exp(est$par[1])),diag(exp(c(est$par[2],est$par[3],est$par[4]))))


filtLevel <- kf$kf$xFilt[1,]
filtSlope <- kf$kf$xFilt[2,]
filtSeason <- kf$kf$xFilt[3,]

predLevel <- kf$kf$xPred[1,]
predSlope <- kf$kf$xPred[2,]
predSeason <- kf$kf$xPred[3,]

smoothLevel <- kf$xSmooth[1,]
smoothSlope <- kf$xSmooth[2,]
smoothSeason <- kf$xSmooth[3,]


par(mfrow = c(3,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(useData,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
lines(index(useData),filtLevel,col = "indianred",lwd = 2)
plot(index(useData),filtSlope,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),filtSlope,col = "indianred",lwd = 2)
plot(index(useData),filtSeason,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),filtSeason,col = "indianred",lwd = 2)


par(mfrow = c(3,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(useData,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
lines(index(useData),predLevel[1:T],col = "indianred",lwd = 2)
legend("topleft",c("Predicted level"), col=c("indianred"),lty=c(1), cex=1.1,box.lty=1)
plot(useData,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
lines(index(useData),filtLevel,col = "indianred",lwd = 2)
legend("topleft",c("Filtered level"), col=c("indianred"),lty=c(1), cex=1.1,box.lty=1)
plot(useData,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
lines(index(useData),smoothLevel,col = "indianred",lwd = 2)
legend("topleft",c("Smoothed level"), col=c("indianred"),lty=c(1), cex=1.1,box.lty=1)



par(mfrow = c(3,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)

plot(useData,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
lines(index(useData),smoothLevel,col = "indianred",lwd = 2)
plot(index(useData),smoothSlope,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),smoothSlope,col = "indianred",lwd = 2)
plot(index(useData),smoothSeason,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),smoothSeason,col = "indianred",lwd = 2)

v <-  t(y - A %*% kf$kf$xPred[,1:T])
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)  
plot(index(useData),v,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),v,lwd = 2)
legend("topright",c("Prediction errors"), col=c("black"),lty=c(1), cex=1.1,box.lty=1)
acf(v,40,lwd = 2)

phi1 <- 1.5
phi2 <- -0.75
x <- arima.sim(list(order = c(2,0,0), ar = c(phi1,phi2), sd = 1),n = 1000)

trend <- lm(HOUSE ~ t)

n <- length(trend$residuals)



periodogram <- Mod(fft(trend$residuals))^2/n

plot(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "n",xlab = expression(omega),ylab = "Periodogram")
grid(lty=1, col = gray(.9))
lines(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "l",lwd = 2)



getSymbols("MRTSSM44X72USN",src = "FRED")

Y <- 100*diff(log(MRTSSM44X72USN$MRTSSM44X72USN))[-1]

Y <- Y[1:(length(Y)-1)]

n <- length(MRTSSM44X72USN)
t = 1:n
acf(Y)



n <- length(Y)
periodogram <- Mod(fft(Y))^2/n
par(mfrow = c(2,1))
plot(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "n",xlab = expression(omega),ylab = "Periodogram")
grid(lty=1, col = gray(.9))
lines(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "l",lwd = 2)
plot(Y)

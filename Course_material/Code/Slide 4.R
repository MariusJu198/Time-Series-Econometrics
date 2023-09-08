rm(list=ls())
#install.packages("openxlsx")
library(openxlsx)
library(zoo)
library(readxl)
library(quantmod)
setwd("C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2022")
savehere <- paste0(getwd(),'/Plots/')
source("Code/Kalman.R")

sig <- sqrt(matrix(c(0.1,1,1,1,1,0.1),3,2))

set.seed(20320224)
for (j in 1:3){ 
  sigEps <- sig[j,1]; sigEta <- sig[j,2] 
  T <- 100
  mu <- matrix(NA,T,1)
  
  mu[1] <- rnorm(1,0,1);
  
  for (t in 2:T){
    mu[t] <- mu[t-1] + sigEta*rnorm(1)  
  }
  
  y <- mu + sigEps*rnorm(T)
  
  
  lims <- c(min(cbind(y,mu)),max(cbind(y,mu)*2))
  legLim <- max(cbind(y,mu)*2)

  pdf(file=paste0(savehere,"LLmodel",j,".pdf"), width=8,height=6)   
  par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
  plot(mu,ylim = lims,lwd = 2, type = "n",ylab = " ",xlab = "Time")
  grid(lty=1, col = gray(.9)); lines(mu,lwd = 2,type = "b")
  lines(y, col = "indianred",ylab = " ", lwd = 2,type = "l",lty = 1)
  legend("topleft",c(expression(y[t]),expression(mu[t])), col=c("indianred","black"),lty=c(1), cex=1.1,lwd = 2)
  dev.off()
  
  
  kf <- KFilterLL(y,sigEta^2, sigEps^2)
  
  
  pdf(file=paste0(savehere,"LLmodelKF",j,".pdf"),width=8,height=6)   
  par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
  plot(mu,ylim = lims,lwd = 2, type = "n",ylab = " ",xlab = "Time")
  grid(lty=1, col = gray(.9))
  lines(mu,type = "b",lwd = 2)
  lines(y, col = "indianred", xlab = "Time (t)",ylab = " ", lwd = 2,type = "l",lty = 1)
  lines(kf$muFilt,col = "steelblue",lwd = 2,lty = 1)
  legend("topleft",c(expression(y[t]),expression(mu[t]),"Filtered states"), col=c("indianred","black","steelblue"),lty=c(1), cex=1.1,lwd = 2)
  dev.off()
  
  
  ks <- KSmooth(t(as.matrix(y)), as.matrix(1), 0, as.matrix(10^7), as.matrix(1), as.matrix(1), as.matrix(sigEps), as.matrix(sigEta))
  
  
  pdf(file=paste0(savehere,"LLmodelKSmooth",j,".pdf"),width=8,height=6)   
  par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
  plot(mu,ylim = lims,lwd = 2, type = "n",ylab = " ",xlab = "Time")
  grid(lty=1, col = gray(.9))
  lines(mu,type = "b",lwd = 2)
  lines(y, col = "indianred", lwd = 2,type = "l",lty = 1)
  lines(as.vector(ks$kf$xFilt),col = "steelblue",lwd = 2,lty = 1)
  lines(as.vector(ks$xSmooth),col = "darkgreen",lwd = 2,lty = 1)
  legend("topleft",c(expression(y[t]),expression(mu[t]),"Filtered states","Smoothed states"), col=c("indianred","black","steelblue","darkgreen"),lty=c(1), cex=1.1,lwd = 2)
  dev.off()
  
  y[50:60] <- NA
  
  ks <- KSmooth(t(as.matrix(y)), as.matrix(1), 0, as.matrix(10^7), as.matrix(1), as.matrix(1), as.matrix(sigEps), as.matrix(sigEta))
  
  pdf(file=paste0(savehere,"LLmodelMissing",j,".pdf"), width=8,height=6)   
  par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
  plot(mu,ylim = lims,lwd = 2, type = "n",ylab = " ",xlab = "Time")
  grid(lty=1, col = gray(.9))
  lines(mu,type = "b",lwd = 2)
  lines(y, col = "indianred", xlab = "Time (t)",ylab = " ", lwd = 2,type = "l",lty = 1)
  lines(as.vector(ks$kf$xFilt),col = "steelblue",lwd = 2,lty = 1)
  lines(as.vector(ks$xSmooth),col = "darkgreen",lwd = 2,lty = 1)
  legend("topleft",c(expression(y[t]),expression(mu[t]),"Filtered states","Smoothed states"), col=c("indianred","black","steelblue","darkgreen"),lty=c(1), cex=1.1,lwd = 2)
  dev.off()
  
}

# CPI ---------------------------------------------------------------------

getSymbols("CPIAUCSL",src="FRED")
dCPI <- 100*diff(log(CPIAUCSL$CPIAUCSL))
dCPI <- zoo(dCPI, as.yearmon(index(CPIAUCSL)))[-1,]
T <- length(dCPI)

func <- function(para){
  kf <- KFilterLL(as.numeric(dCPI$CPIAUCSL),para[1], para[2])
  return(-kf$like) 
}



est <- optim(c(.1,.01), func, gr=NULL, method='BFGS', hessian=FALSE,
             control=list(trace=1, REPORT=1,maxit = 50000))
kf <- KFilterLL(dCPI,est$par[1], est$par[2]) # The optimal filtered states

pdf(file=paste0(savehere,"CPI.pdf"), width=8,height=6)  
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(dCPI,ylim = c(-2,3),lwd = 2, type = "n",ylab = "",xlab = "")
grid(lty=1, col = gray(.9))
lines(dCPI,type = "l",lwd = 2)
lines(index(dCPI),kf$muFilt,type = "l",lwd = 2,col = "indianred")
legend("topleft",c("Log changes in CPI","Filtered states"), col=c("black","indianred"),lty=c(1,1), cex=1.1)
dev.off()

pdf(file=paste0(savehere,"CPI_ACF.pdf"), width=8,height=6)  
par(mfrow = c(2,1), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(dCPI - kf$muPred[1:T],ylim = c(-2,3),lwd = 2, type = "n",ylab = "",xlab = "")
grid(lty=1, col = gray(.9))
lines(dCPI - kf$muPred[1:T],type = "l",lwd = 2)
acf(as.numeric(dCPI - kf$muPred[1:T]),40,col = "black",lwd = 2)
dev.off()


# AR(1) -------------------------------------------------------------------
set.seed(20320224)
AR <- arima.sim(n = 1000, list(ar = c(0.95)), sd = sqrt(0.2))
AR2 = ts.intersect(AR , AR1=lag(AR,-1),dframe=TRUE)
summary(fit <- lm(AR ~ AR1 + 0, data=AR2, na.action=NULL))
sigma2 <- var(fit$residuals)

y <- t(AR);	T <- ncol(y) 
A <- as.matrix(1);	mu0 <- as.matrix(0)
Theta <- as.matrix(1);	cR <- as.matrix(0)

func <- function(par){
  Phi <- par[1]; cQ <- exp(par[2]); Sigma0 <- 10^17
  kf <- KFilter((y),A,mu0,Sigma0,Phi,Theta,cR,cQ)
  return(-kf$like) 
}
est <- optim(c(0.95,log(sqrt(0.25))), func, gr=NULL, method='BFGS', hessian=TRUE,
             control=list(trace=1, REPORT=1,maxit = 50000))
SE <- sqrt(diag(solve(est$hessian)))
rbind(cbind(est$par[1],exp(est$par[2]*2)),cbind(fit$coefficients,var(fit$residuals)))

# Unemployment ------------------------------------------------------------

getSymbols("UNRATENSA",src="FRED") #Requires quantmod to be loaded
DATA <- zoo(UNRATENSA$UNRATENSA, as.yearmon(index(UNRATENSA)))
useData <- log((DATA[1:(length(DATA)-32),]))

pdf(file=paste0(savehere,"unempKF.pdf"), width=8,height=5)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(useData,xlab = "Year",ylab = "Log unemployment rate (%)",type = "n")
grid(lty=1, col = gray(.9))
lines(useData,lwd = 2)
dev.off()


y <- t(as.numeric(useData))
T <- ncol(y) 
A <- cbind(1,0,1,0,0,0,0,0,0,0,0,0,0)
mu0 <- as.matrix(rep(0,13)); mu0[1] <- y[1]
Sigma0 <- diag(10^7,13)
Phi <- rbind(c(1,1,rep(0,11)),c(0,1,rep(0,11)),c(0,0,rep(-1,11)),cbind(rep(0,10),rep(0,10),diag(1,10),rep(0,10)))
Theta <- cbind(c(1,rep(0,12)),c(0,1,rep(0,11)),c(0,0,1,rep(0,10)))

func <- function(par){
  kf <- KFilter(y,A,mu0,Sigma0,Phi,Theta,as.matrix(exp(par[1])),diag(exp(c(par[2],par[3],par[4]))))
  return(-kf$like) 
}

est <- optim(c(-8, -3, -5, -5), func, gr=NULL, method='BFGS', hessian=FALSE,
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


pdf(file=paste0(savehere,"UnempLTS.pdf"), width=8,height=6)  
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
dev.off()


pdf(file=paste0(savehere,"UnempLTS_Levels.pdf"), width=8,height=6)  
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
dev.off()



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


pdf(file=paste0(savehere,"UnempLTS_Residuals.pdf"), width=8,height=6)  
v <-  t(y - A %*% kf$kf$xPred[,1:T])
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)  
plot(index(useData),v,type = "n",xlab = "Year",ylab = "")
grid(lty=1, col = gray(.9))
lines(index(useData),v,lwd = 2)
legend("topright",c("Prediction errors"), col=c("black"),lty=c(1), cex=1.1,box.lty=1)
acf(v,40,lwd = 2)
dev.off()

# True out-of-sample Forecasting 
nAhead <- 32
newY <- matrix(NA,T+nAhead,1)
newY[1:T] <- y
kf <- KSmooth(t(newY),A,mu0,Sigma0,Phi,Theta,as.matrix(exp(est$par[1])),diag(exp(c(est$par[2],est$par[3],est$par[4]))))


forecastDirect <- A %*% kf$kf$xPred[,(T+1):(T+nAhead)]
forecastDirect <- ts(t(forecastDirect),start = c(2020,1),frequency = 12)

conf <- matrix(NA,T+nAhead,2)
conf[(T+1):(T+nAhead),1] <- forecastDirect + 2*sqrt(kf$kf$F[(T+1):(T+nAhead)])
conf[(T+1):(T+nAhead),2] <- forecastDirect - 2*sqrt(kf$kf$F[(T+1):(T+nAhead)])
conf <- ts(conf,start = c(1948,1),frequency = 12)

newY[(T+1):(T+nAhead)] <- forecastDirect
newY <- ts(newY,start = c(1948,1),frequency = 12)
oldY <- newY
oldY[(T+1):(T+nAhead)] <- NA

newY[1:T] <- NA
tsDATA <- ts(DATA$UNRATENSA, start = c(1948,1), frequency = 12 )

pdf(file=paste0(savehere,"UnempLTS_Forecast.pdf"), width=8,height=6) 
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
ts.plot(log(tsDATA),newY,conf,lty = c(1,1,1,1), lwd = c(2,2,2,2),col = c("black","indianred","steelblue","steelblue"),xlab = "")
grid()
legend("topleft",c("Actual","Forecast","95% confidence bands"), col=c("black","indianred","steelblue"),lty=c(1,1,1), cex=1.1,lwd = 2)
dev.off()

# Pseudo out-of-sample Forecasting
nAhead <- 24
yEst <- ts(t(as.matrix(useData[1:(T-nAhead),1])),start = c(1948, 1), frequency = 12)

func <- function(par){
  kf <- KFilter(yEst,A,mu0,Sigma0,Phi,Theta,as.matrix(exp(par[1])),diag(exp(c(par[2],par[3],par[4]))))
  return(-kf$like) 
}

est <- optim(c(-8.793457, -3.265107, -5.153260, -5.061522), func, gr=NULL, method='BFGS', hessian=FALSE,
             control=list(trace=1, REPORT=1,maxit = 50000))

forec <- matrix(NA,nAhead,2)

for (j in 1:nAhead){
  useY <- y[1:(T-nAhead+j-1)]
  useY <- ts(t(append(t(useY),NA)),start = c(1948, 1),frequency = 12)
  kf <- KSmooth(useY,A,mu0,Sigma0,Phi,Theta,as.matrix(exp(est$par[1])),diag(exp(c(est$par[2],est$par[3],est$par[4]))))
  forec[j,1] <- A %*% kf$kf$xPred[,T-nAhead+j]
  forec[j,2] <- kf$kf$F[T-nAhead+j]
}

forecast <- ts(forec[,1],start = c(2018,1),frequency = 12)
F <- ts(forec[,2],start = c(2018,1),frequency = 12)


pdf(file=paste0(savehere,"UnempLTS_ForecastPseudoRolling.pdf"), width=8,height=6) 
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
ts.plot(useData[(T-nAhead+1):T],forecast,forecast + 2*sqrt(F),forecast - 2*sqrt(F),lty = c(2,2,1,1), lwd = 2,col = c("black","indianred","steelblue","steelblue"),
        ylim = c(min(forecast - 2*sqrt(F)),max(forecast + 2*sqrt(F))*1.2),xlab = "")
grid()
legend("topleft",c("Actual","Forecast","95% confidence bands"), col=c("black","indianred","steelblue"),lty=c(2,2,1), cex=1.1,lwd = 2)
dev.off()

# Pseudo out-of-sample Forecasting - direct
yDirect <- matrix(NA,T,1)
yDirect[1:(T-nAhead)] <- yEst

kf <- KSmooth(t(yDirect),A,mu0,Sigma0,Phi,Theta,as.matrix(exp(est$par[1])),diag(exp(c(est$par[2],est$par[3],est$par[4]))))

forecastDirect <- A %*% kf$kf$xPred[,((T-nAhead)+1):(T)]
forecastDirect <- ts(t(forecastDirect),start = c(2018,1),frequency = 12)

conf <- matrix(NA,nAhead,2)
conf[,1] <- forecastDirect + 2*sqrt(kf$kf$F[(T-nAhead+1):T])
conf[,2] <- forecastDirect - 2*sqrt(kf$kf$F[(T-nAhead+1):T])
conf <- ts(conf,start = c(2018,1),frequency = 12)

pdf(file=paste0(savehere,"UnempLTS_ForecastPseudoDirect.pdf"), width=8,height=6) 
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
ts.plot(y[(T-nAhead+1):T],forecastDirect,conf,lty = c(2,2,1,1), lwd = 2,col = c("black","indianred","steelblue","steelblue"))
grid()
legend("topleft",c("Actual","Forecast","95% confidence bands"), col=c("black","indianred","steelblue"),lty=c(2,2,1), cex=1.1,lwd = 2)
dev.off()


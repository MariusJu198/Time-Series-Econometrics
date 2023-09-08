rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)

#Set working directory
setwd('C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2022/')


#Specify where we want to save output
savehere <- paste0(getwd(),'/Plots/')


T <- 250
# Simulation of white noise process
set.seed(123)
w <- rnorm(T)
wt <- rt(T,5)

pdf(file=paste0(savehere,"wn.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(1:T,w,type = "n",ylab = "",xlab = "Time",main = bquote(bold("Gaussian iid noise "~X[t]~"~ N(0,1)")))
grid(lty=1, col = gray(.9))
lines(w,lwd = 2,col = "steelblue")
plot(1:T,wt,type = "n", ylab = "",xlab = "Time",main = bquote(bold("Student-t iid noise "~X[t]~"~ t(5)")))
grid(lty=1, col = gray(.9))
lines(wt,lwd = 2,col = "steelblue")
dev.off()

arCoeffs <- c(0.2,0.5,0.9,-0.9)

for (j in 1:length(arCoeffs)){
    pdf(file=paste0(savehere,paste0("AR_",j,".pdf")), width=8,height=6) 
    par(mfrow = c(2,2), mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
    plot(w,type = "n",xlab = "Time",ylab = "", main =  bquote("Gaussian white noise"))
    grid(lty=1, col = gray(.9))
    lines(w,lwd = 2,col = "steelblue")
    plot(0:50,0^(0:50),type = "n",ylab = "ACF",xlab = "Lag")
    grid(lty=1, col = gray(.9))
    lines(0:50,0^(0:50),lwd = 2,col = "steelblue",type = "h")
    ar <- arima.sim(model = list(ar = arCoeffs[j],order = c(1,0,0)), n = 250, innov = w)
    plot(ar,type = "n",xlab = "Time",ylab = "", main = bquote(bold("AR(1) with"~phi~"="~.(arCoeffs[j]))))
    grid(lty=1, col = gray(.9))
    lines(ar,lwd = 2,col = "steelblue")
    plot(0:50,arCoeffs[j]^(0:50),type = 'n',ylab = "ACF",xlab = "Lag")
    grid(lty=1, col = gray(.9))
    lines(0:50,arCoeffs[j]^(0:50),lwd = 2,col = "steelblue",type = "h")
    dev.off()
}

pdf(file=paste0(savehere,paste0("RW.pdf")), width=8,height=6) 
par(mfrow = c(2,1), mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(w,type = "n",xlab = "Time",ylab = "", main =  bquote("Gaussian white noise"))
grid(lty=1, col = gray(.9))
lines(w,lwd = 2,col = "steelblue")
plot(cumsum(w),type = "n",xlab = "Time",ylab = "", main = "Random walk")
grid(lty=1, col = gray(.9))
lines(cumsum(w),lwd = 2,col = "steelblue")
dev.off()



maCoeffs <- c(0.2,0.5,0.9,-0.9)

for (j in 1:length(maCoeffs)){
  pdf(file=paste0(savehere,paste0("MA_",j,".pdf")), width=8,height=6) 
  par(mfrow = c(2,2), mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0), cex.main=1.05)
  plot(w,type = "n",xlab = "Time",ylab = "", main =  bquote("Gaussian white noise"))
  grid(lty=1, col = gray(.9))
  lines(w,lwd = 2,col = "steelblue")
  plot(0:50,0^(0:50),type = "n",ylab = "ACF",xlab = "Lag")
  grid(lty=1, col = gray(.9))
  lines(0:50,0^(0:50),lwd = 2,col = "steelblue",type = "h")
  ma <- arima.sim(model = list(ma = maCoeffs[j],order = c(0,0,1)), n = 250, innov = w)
  plot(ma,type = "n",xlab = "Time",ylab = "", main = bquote(bold("MA(1) with"~theta~"="~.(maCoeffs[j]))))
  grid(lty=1, col = gray(.9))
  lines(ma,lwd = 2,col = "steelblue")
  plot(0:50,c(1,maCoeffs[j]/(maCoeffs[j]^2+1),rep(0,49)),type = 'n',ylab = "ACF",xlab = "Lag")
  grid(lty=1, col = gray(.9))
  lines(0:50,c(1,maCoeffs[j]/(maCoeffs[j]^2+1),rep(0,49)),lwd = 2,col = "steelblue",type = "h")
  dev.off()
}

#Simulation study

N <- c(100,1000,10000)
sim <- 10000
for (nn in 1:length(N)){
  res <- matrix(NA,20+1,sim)
  for (s in 1:sim){
    w <- rnorm(N[nn])
    ar <- arima.sim(model = list(ar = 0.95,order = c(1,0,0)), n = N[nn],innov = w)
    acf <- acf(ar,lag.max = 20,plot = FALSE)
    res[,s] <- acf$acf
  }
  avgACF <- rowMeans(res)
  theoACF <- 0.95^(0:20)
  pdf(file=paste0(savehere,paste0("acfBias_N",N[nn],".pdf")), width=8,height=6)  
  par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
  plot(0:20,avgACF,type = "n",xlab = "Lag (h)",ylab = "ACF",ylim = c(-0.02,1))
  grid(lty=1, col = gray(.9))
  lines(0:20,theoACF,type = "h",lwd = 2, col = "black")
  lines(0:20,avgACF,type = "h",lwd = 2, col = "red")
  legend("topright",c("Theoretical ACF","Average sample ACF"), col=c("black","red"),lty = 1,lwd = 2, cex=1,box.lty=1)
  dev.off()
}

t <- 1:length(globtemp)

summary(fit <- lm(globtemp ~ t + I(t^2)))

Box.test(fit$residuals, lag = 20, type = c("Ljung-Box"), fitdf = 3)
ar <- ar.ols(fit$residuals,FALSE,order = 1)
Box.test(ar$resid, lag = 20, type = c("Ljung-Box"), fitdf = 5)

pdf(file=paste0(savehere,"residTest.pdf"), width=8,height=5) 
par(mfrow = c(3,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(globtemp,type = "n",ylab = "",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(globtemp,lwd = 2,type = "o")
lines(index(globtemp),fit$fitted.values,lwd = 2,type = "l",col = "steelblue")
legend("topleft",c("Temperature deviations","Fitted trend"), col=c("black","steelblue"),lty = 1,lwd = 2, cex=1)
plot(index(globtemp),fit$residuals,xlab = "Year",ylab = "Residuals",type = "n")
grid(lty=1, col = gray(.9))
lines(index(globtemp),fit$residuals,lwd = 2, col = "steelblue")
plot(index(globtemp),ar$resid,xlab = "Year",ylab = "AR(1) residuals",type = "n")
grid(lty=1, col = gray(.9))
lines(index(globtemp),ar$resid,lwd = 2, col = "red")
dev.off()

pdf(file=paste0(savehere,"residACF.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)

acf(fit$residuals,lwd = 2)
acf(ar$resid[-1],lwd = 2)
dev.off()

    
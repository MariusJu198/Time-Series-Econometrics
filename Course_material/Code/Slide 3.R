rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)

#Set working directory
setwd('C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2021/')


#Specify where we want to save output
savehere <- paste0(getwd(),'/Plots/')



ARMAtoMA(ar = 0.9,ma = 0.5, 20)
ARMAtoAR(ar = 0.9,ma = 0.5, 20)

ac <- function(r1,r2,sigma2,h){

  tmp <- (r1^2 * r2^2)/((r1*r2-1)*(r2-r1))
  tmp <- tmp *( r1^(1-h)/(r1^2 - 1) - r2^(1-h)/(r2^2 -1))*sigma2

  return (list(f = tmp))
}

sigma2 = 1; lags <- 0:20
pdf(file=paste0(savehere,"AR2ACF.pdf"), width=9,height=4)   
par(mfrow = c(1,2), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
r1 <- 2; r2 <- 5; 
acf <- ac(r1,r2,sigma2,lags)$f/ac(r1,r2,sigma2,0)$f

plot(acf,type = "n",ylab = "ACF",xlab = "Lag")
grid(lty=1, col = gray(.9))
lines(acf,lwd = 5,col = "steelblue",type = "h")
abline(0,0)

r1 <- 1+1i/sqrt(3); r2 <- 1-1i/sqrt(3);
acf <- Re(ac(r1,r2,sigma2,lags)$f/ac(r1,r2,sigma2,0)$f)

plot(acf,type = "n",ylab = "ACF",xlab = "Lag")
grid(lty=1, col = gray(.9))
lines(acf,lwd = 5,col = "steelblue",type = "h")
abline(0,0)
dev.off()



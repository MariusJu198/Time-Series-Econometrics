rm(list=ls())
library(zoo)
library(latex2exp)
library(quantmod)
library(astsa)
#Set working directory
setwd("C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2022/")
# Change this line to a directory on your own computer!

#Specify where we want to save output
savehere <- paste0(getwd(),"/Plots/")

t <- seq(0,2*pi,0.001)
x1 <- cos(2*pi*t)
x2 <- cos(t)

pdf(file=paste0(savehere,"cosines.pdf"), width=8,height=5)   
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",main = expression(omega == 1))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
plot(t,x2,xlab = "",ylab = "",type = "n",main = expression(lambda == 1))
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2,col = "steelblue")
dev.off()


t <- seq(0,2*pi,0.001)
x1 <- cos(0.5*t)
x2 <- cos(t)
x3 <- cos(2*t)
x4 <- cos(4*t)

pdf(file=paste0(savehere,"cosinesLambda.pdf"), width=8,height=5)   
par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",main = expression(lambda == 1/2))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
plot(t,x2,xlab = "",ylab = "",type = "n",main = expression(lambda == 1))
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2,col = "steelblue")
plot(t,x3,xlab = "",ylab = "",type = "n",main = expression(lambda == 2))
grid(lty=1, col = gray(.9))
lines(t,x3,lwd = 2,col = "steelblue")
plot(t,x4,xlab = "",ylab = "",type = "n",main = expression(lambda == 4))
grid(lty=1, col = gray(.9))
lines(t,x4,lwd = 2,col = "steelblue")
dev.off()


t <- seq(0,10,0.001)
x1 <- cos(2*pi*t*(5/100))
x2 <- cos(2*pi*t*(10/100))
x3 <- cos(2*pi*t*(25/100))
x4 <- cos(2*pi*t*(50/100))

pdf(file=paste0(savehere,"cosinesOmega.pdf"), width=8,height=5)   
par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",main = expression(omega == 1/20))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
plot(t,x2,xlab = "",ylab = "",type = "n",main = expression(omega == 1/10))
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2,col = "steelblue")
plot(t,x3,xlab = "",ylab = "",type = "n",main = expression(omega == 1/4))
grid(lty=1, col = gray(.9))
lines(t,x3,lwd = 2,col = "steelblue")
plot(t,x4,xlab = "",ylab = "",type = "n",main = expression(omega == 1/2))
grid(lty=1, col = gray(.9))
lines(t,x4,lwd = 2,col = "steelblue")
dev.off()





t <- seq(0,10,0.01)
x1 <- 0.5*cos(2*pi*t*(50/100))
x2 <- cos(2*pi*t*(50/100))
x3 <- 1.5*cos(2*pi*t*(50/100))
x4 <- 2*cos(2*pi*t*(50/100))

pdf(file=paste0(savehere,"cosinesA.pdf"), width=8,height=5)   
par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",main = expression(A == 1/2),ylim = c(-2,2))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
plot(t,x2,xlab = "",ylab = "",type = "n",main = expression(A == 1),ylim = c(-2,2))
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2,col = "steelblue")
plot(t,x3,xlab = "",ylab = "",type = "n",main = expression(A == 3/2),ylim = c(-2,2))
grid(lty=1, col = gray(.9))
lines(t,x3,lwd = 2,col = "steelblue")
plot(t,x4,xlab = "",ylab = "",type = "n",main = expression(A == 2),ylim = c(-2,2))
grid(lty=1, col = gray(.9))
lines(t,x4,lwd = 2,col = "steelblue")
dev.off()


t <- seq(0,10,0.01)
x1 <- cos(2*pi*t*(50/100))
x2 <- cos(2*pi*t*(50/100) + pi/2)
x3 <- cos(2*pi*t*(50/100) + pi)
x4 <- cos(2*pi*t*(50/100) + 3/2*pi)

pdf(file=paste0(savehere,"cosinesPhi.pdf"), width=8,height=5)   
par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",main = expression(phi == 0),ylim = c(-1,1))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
plot(t,x2,xlab = "",ylab = "",type = "n",main = expression(phi == pi/2),ylim = c(-1,1))
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2,col = "steelblue")
plot(t,x3,xlab = "",ylab = "",type = "n",main = expression(phi == pi),ylim = c(-1,1))
grid(lty=1, col = gray(.9))
lines(t,x3,lwd = 2,col = "steelblue")
plot(t,x4,xlab = "",ylab = "",type = "n",main = expression(phi == (3/2)*pi),ylim = c(-1,1))
grid(lty=1, col = gray(.9))
lines(t,x4,lwd = 2,col = "steelblue")
dev.off()


t <- seq(0,4,0.01)
x1 <- cos(2*pi*t*(1/2))
x2 <- cos(2*pi*t*(3/2))




pdf(file=paste0(savehere,"aliasing.pdf"), width=8,height=4.5)   
par(mfrow = c(1,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1,xlab = "",ylab = "",type = "n",ylim = c(-1,1.5))
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2,col = "steelblue")
lines(t,x2,lwd = 2,col = "darkgreen")
legend("topleft",c(expression(X[t]==cos(2*pi*0.5*t)),expression(X[t]==cos(2*pi*1.5*t))), col=c("steelblue","darkgreen"),lty=c(1), cex=1.1,lwd = 2,box.lty=1)
dev.off()


n <- 100
t <- seq(1,n,1)
x1 <- 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x2 <- 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x3 <- 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
x <- x1 + x2 + x3
pdf(file=paste0(savehere,"sumCosines.pdf"), width=8,height=4.5)   
par(mfrow = c(2,2), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(t,x1, main=expression(omega==6/100~~~A^2==13),ylim=c(-10,10),xlab = "",ylab = TeX("$X_{1}$"),type = "n")
grid(lty=1, col = gray(.9))
lines(t,x1,lwd = 2, col = "steelblue",type = "l")

plot(t,x2, main=expression(omega==10/100~~~A^2==41),ylim=c(-10,10),xlab = "",ylab = TeX("$X_{2}$"),type = "n")
grid(lty=1, col = gray(.9))
lines(t,x2,lwd = 2, col = "steelblue",type = "l")

plot(t,x3, main=expression(omega==40/100~~~A^2==85),ylim=c(-10,10),xlab = "",ylab = TeX("$X_{3}$"),type = "n")
grid(lty=1, col = gray(.9))
lines(t,x3,lwd = 2, col = "steelblue",type = "l")

plot(t,x,  main=TeX("$X_t = X_{t1}+X_{t2}+X_{t3}$"),ylim=c(-16,16),xlab = "",ylab = TeX("$X$"),type = "n")
grid(lty=1, col = gray(.9))
lines(t,x,lwd = 2, col = "steelblue",type = "l")
dev.off()

pdf(file=paste0(savehere,"scaledPeriodogram.pdf"), width=8,height=4.5)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot((0:(n-1))/n,Mod(2*fft(x)/n)^2,type = "n",xlab = TeX("Frequency ($\\omega$)"),ylab = "Scaled periodogram")
grid(lty=1, col = gray(.9))
lines((0:(n-1))/n,Mod(2*fft(x)/n)^2,type = "o",lwd = 2)
abline(v = 0.5, col="black", lwd=2, lty=2)
dev.off()


getSymbols("HOUSTNSA",src="FRED") #Requires quantmod to be loaded
HOUSE <- zoo(HOUSTNSA$HOUSTNSA, as.yearmon(index(HOUSTNSA)))
x <- diff(HOUSE$HOUSTNSA,1)[-1]
n <- length(x)
periodogram <- Mod(fft(x))^2/n
scaledPeriodogram <- Mod(2*fft(x)/n)^2



pdf(file=paste0(savehere,"periodograms.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)

plot.ts(x,type = "n",ylab = "")
grid(lty=1, col = gray(.9))
lines(x,lwd = 2)

plot(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "n",xlab = TeX("Frequency ($\\omega$)"),ylab = "Periodogram")
grid(lty=1, col = gray(.9))
lines(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "o",lwd = 2)
text(0.21,17500,"Biannual cycle");text(0.05,12500,"Yearly cycle")
dev.off()

omega <- seq(0,0.5,0.001)
phi <- 0.9
sp1 <- (1-2*phi*cos(2*pi*omega)+phi^2)^(-1)

phi <- -0.9
sp2 <- (1-2*phi*cos(2*pi*omega)+phi^2)^(-1)

pdf(file=paste0(savehere,"arSpec.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))

plot(omega,sp1,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\phi = 0.9$"))
grid(lty=1, col = gray(.9))
lines(omega,sp1,lwd = 2)

plot(omega,sp2,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\phi = -0.9$"))
grid(lty=1, col = gray(.9))
lines(omega,sp2,lwd = 2)
dev.off()



pdf(file=paste0(savehere,"arACFsp.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))


plot(0:40,0.9^(0:40),type = "n",ylab = "",xlab = "Lag",main = TeX("$\\phi = 0.9$"))
grid(lty=1, col = gray(.9))
lines(0:40,0.9^(0:40),lwd = 4,type = "h")

plot(0:40,(-0.9)^(0:40),type = "n",ylab = "",xlab = "Lag",main = TeX("$\\phi = 0.9$"))
grid(lty=1, col = gray(.9))
lines(0:40,(-0.9)^(0:40),lwd = 4,type = "h")
dev.off()


theta <- 0.5
sp1 <- (1+2*theta*cos(2*pi*omega)+theta^2)

theta  <- -0.5
sp2 <- (1+2*theta*cos(2*pi*omega)+theta^2)

pdf(file=paste0(savehere,"maSpec.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))

plot(omega,sp1,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\theta = 0.5$"))
grid(lty=1, col = gray(.9))
lines(omega,sp1,lwd = 2)

plot(omega,sp2,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\theta = -0.5$"))
grid(lty=1, col = gray(.9))
lines(omega,sp2,lwd = 2)
dev.off()


phi1 <- 1.5
phi2 <- -0.75

sp <- (1 + phi1^2 + phi2^2 -2*phi2*cos(4*pi*omega)- 2*phi1*cos(2*pi*omega)+2*phi1*phi2*cos(2*pi*omega))^(-1)
pdf(file=paste0(savehere,"ar2Spec.pdf"), width=8,height=5) 
par(mfrow = c(2,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(omega,sp,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\phi_1 = 1.5,\\; \\phi_2 = -0.75$"))
grid(lty=1, col = gray(.9))
lines(omega,sp,lwd = 2)

phi1 <- 1
phi2 <- -0.9

sp <- (1 + phi1^2 + phi2^2 -2*phi2*cos(4*pi*omega)- 2*phi1*cos(2*pi*omega)+2*phi1*phi2*cos(2*pi*omega))^(-1)

plot(omega,sp,type = "n",ylab = "",xlab = expression(omega),main = TeX("$\\phi_1 = 1,\\; \\phi_2 = -0.9$"))
grid(lty=1, col = gray(.9))
lines(omega,sp,lwd = 2)
dev.off()


set.seed(1234)
x <- arima.sim(list(order = c(2,0,0), ar = c(phi1,phi2), sd = 1),n = 1000)
n <- length(x)

periodogram <- Mod(fft(x))^2/n
sp <- (1 + phi1^2 + phi2^2 -2*phi2*cos(4*pi*omega)- 2*phi1*cos(2*pi*omega)+2*phi1*phi2*cos(2*pi*omega))^(-1)

pdf(file=paste0(savehere,"smoothPer.pdf"), width=8,height=5) 
par(mfrow = c(3,1), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))

plot(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "n",xlab = expression(omega),ylab = "Periodogram")
grid(lty=1, col = gray(.9))
lines(seq(0,(n-1)/(2*n),1/n),periodogram[1:(n/2)],type = "l",lwd = 2)

mvspec(x,log = "no",kernel = kernel("modified.daniell", c(3,3)),taper = 0.1,lwd = 2,main = "",xlab = expression(omega),ylab = "Periodogram")
plot(omega,sp,type = "n",ylab = "Periodgram",xlab = expression(omega),main = "")
grid(lty=1, col = gray(.9))
lines(omega,sp,lwd = 2)
dev.off()



rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)

#Set working directory
setwd("C:/Users/au282988/Dropbox/Postdoc/Teaching/Time Series Econometrics E2022/")
# Change this line to a directory on your own computer!


#Specify where we want to save output
savehere <- paste0(getwd(),"/Plots/")

#US GDP
getSymbols("GDPC1",src="FRED") #Requires quantmod to be loaded
GDP <- zoo(GDPC1$GDPC1, as.yearqtr(index(GDPC1), format = "%Y:0%q"))

pdf(file=paste0(savehere,"gdp.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(GDP,xlab = "Year",ylab = "Billions $", type = "n")
grid(lty=1, col = gray(.9)); lines(GDP,lwd = 2)
dev.off()


# Unemployment rate
getSymbols("UNRATENSA",src="FRED") #Requires quantmod to be loaded
UNRATE <- zoo(UNRATENSA$UNRATENSA, as.yearmon(index(UNRATENSA)))


pdf(file=paste0(savehere,"unemp.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(UNRATE,xlab = "Year",ylab = "US unemployment rate (%)",type = "n",ylim = c(1,max(UNRATE)))
grid(lty=1, col = gray(.9))
lines(UNRATE,lwd = 2)
dev.off()


#Housing starts
getSymbols("HOUSTNSA",src="FRED") #Requires quantmod to be loaded
HOUSE <- zoo(HOUSTNSA$HOUSTNSA, as.yearmon(index(HOUSTNSA)))

pdf(file=paste0(savehere,"house.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(HOUSE,xlab = "Year",ylab = "Housing starts in thousands", type = "n",ylim = c(1,max(HOUSE)))
grid(lty=1, col = gray(.9))
lines(HOUSE,lwd = 2)
dev.off()

# DJIA stock returns data
getSymbols("^DJI", src = "yahoo", from = "2001-01-01") #Requires quantmod to be loaded
DJ <- zoo(DJI$DJI.Close, index(DJI))
r <- 100*diff(log(DJ))[-1]

pdf(file=paste0(savehere,"djia.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(DJ,type = "n",ylab = "Closing price",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(DJ,lwd = 2)
plot(r,type = "n",ylab = "Daily stock returns (%)",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(r,lwd = 2)
dev.off()

# Sales

getSymbols("MRTSSM44X72USN",src = "FRED")
MR <- zoo(MRTSSM44X72USN$MRTSSM44X72USN, index(MRTSSM44X72USN))

pdf(file=paste0(savehere,"RetailSales.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(MR,type = "n",ylab = "Sales",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(MR,lwd = 2)
plot(log(MR),type = "n",ylab = "Log sales",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(log(MR),lwd = 2)
dev.off()

# Deaths
tmp <- read_xlsx(paste0(getwd(),"/Data/deaths.xlsx"));  # Load the data
tmp$Date <- as.yearmon(paste(tmp$Y, tmp$M), "%Y %m")
deaths <- zoo(tmp$D,tmp$Date)

pdf(file=paste0(savehere,"deaths.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(deaths, type = "n",ylab = "Deaths", ylim = c(6500,12000),xlab = "Year")
grid(lty=1, col = gray(.9))
lines(deaths,lwd = 2,type = "o")
dev.off()

# Temperature data
pdf(file=paste0(savehere,"globtemp.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(globtemp,type = "n",ylab = "Global temperature deviations",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(globtemp,lwd = 2,type = "o")
dev.off()

# IID Noise
set.seed(123)
noise <- rnorm(200)
pdf(file=paste0(savehere,"iidNoise.pdf"), width=8,height=6)   
par(mfrow = c(1,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(noise,type = "n",ylab =  expression(x[t]),xlab = "Time")
grid(lty=1, col = gray(.9))
lines(noise,lwd = 2, type = "o")
dev.off()



# Trend estimation for US GDP series
getSymbols("GDPC1",src="FRED") #Load the data- requires "quantmod" to be loaded
GDP <- zoo(GDPC1$GDPC1, as.yearqtr(index(GDPC1), format = "%Y:0%q"))

# Linear trend
trend <- 1:length(GDP)
summary(fit <- lm(GDP ~ trend)) #OLS estimation 

pdf(file=paste0(savehere,"gdpTrend.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2) #Graphic settings
plot(GDP,xlab = "Year",ylab = "US GDP",type = "n",ylim = c(0,20000))
grid(lty=1, col = gray(.9))
lines(GDP,lwd = 2)
lines(fit$fitted.values,col = "steelblue",lwd = 2)
legend("topleft" ,c("US GDP","Linear trend"), col=c("black","steelblue"),lty=c(1,1),lwd = 2, cex=1,box.lty=1)
plot(fit$residuals,xlab = "Year",ylab = "Residuals",type = "n",col = "steelblue")
grid(lty=1, col = gray(.9))
lines(fit$residuals,lwd = 2, col = "steelblue")
lines(index(GDP),rep(0,length(GDP)),col = "black",lty = 2, lwd = 1)
dev.off()

# Quadratic trend
summary(fit2 <- lm(GDP ~ trend + I(trend^2))) # Quadratic trend

pdf(file=paste0(savehere,"gdpTrend2.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(GDP,xlab = "Year",ylab = "US GDP",type = "n")
grid(lty=1, col = gray(.9))
lines(GDP,lwd = 2)
lines(fit2$fitted.values,col = "steelblue",lwd = 2)
legend("topleft",c("US GDP","Quadratic trend"), col=c("black","steelblue"),lty=c(1,1),lwd = 2, cex=1,box.lty=1)
plot(fit2$residuals,xlab = "Year",ylab = "Residuals",type = "n",col = "steelblue")
grid(lty=1, col = gray(.9))
lines(fit2$residuals,lwd = 2, col = "steelblue")
lines(index(GDP),rep(0,length(GDP)),col = "black",lty = 2, lwd = 1)
dev.off()


# Log-linear trend

logGDP <- log(GDP)
summary(fitLog <- lm(logGDP ~ trend)) # Log-linear trend estimation

pdf(file=paste0(savehere,"gdpTrendLog.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(logGDP,xlab = "Year",ylab = "Log US GDP",type = "n",ylim = c(7.5,10))
grid(lty=1, col = gray(.9))
lines(logGDP,lwd = 2)
lines(fitLog$fitted.values,col = "steelblue",lwd = 2)
legend("topleft" ,c("Log US GDP","Linear trend"), col=c("black","steelblue"),lty=c(1,1),lwd = 2, cex=1,box.lty=1)
plot(fitLog$residuals,xlab = "Year",ylab = "Residuals", type = "n")
grid(lty=1, col = gray(.9))
lines(fitLog$residuals,lwd = 2, col = "steelblue")
lines(index(GDP),rep(0,length(GDP)),col = "black",lty = 2, lwd = 1)
dev.off()

# Unemployment rate
getSymbols("UNRATENSA",src="FRED") #Requires quantmod to be loaded
UNRATE <- zoo(UNRATENSA$UNRATENSA, as.yearmon(index(UNRATENSA)))

M <- factor(cycle(UNRATE) ) 
summary(fit <- lm(UNRATE ~ time(UNRATE) + M))

pdf(file=paste0(savehere,"unempFitted.pdf"), width=8,height=6)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(UNRATE,xlab = "Year",ylab = "US unemployment rate", type = "n",ylim = c(1,max(UNRATE)))
grid(lty=1, col = gray(.9))
lines(UNRATE,lwd = 2)
lines(fit$fitted.values,col = "steelblue",lwd = 2)
legend("topleft",c("Unemployment rate","Fitted trend and season"), col=c("black","steelblue"),lty = 1,lwd = 2, cex=1)
plot(fit$residuals,xlab = "Year",ylab = "Residuals",type = "n")
grid(lty=1, col = gray(.9))
lines(fit$residuals,lwd = 2, col = "steelblue")
lines(index(UNRATE),rep(0,length(UNRATE)),col = "black",lty = 2, lwd = 1)
dev.off()

# Deaths
T <- length(deaths[,3])
t <- 1:T
M <- factor(cycle(deaths)) 
summary(fit <- lm(deaths ~ t + I(t^2) + M ))

pdf(file=paste0(savehere,"deathEstDummy.pdf"), width=8,height=5.1)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(deaths, type = "n",ylab = "Deaths", ylim = c(6500,12500),xlab = "Year")
grid(lty=1, col = gray(.9))
lines(deaths,lwd = 2,type = "o")
lines(fit$fitted.values, col = "steelblue", lwd = 2)
lines(index(deaths),fit$coefficients[1] + fit$coefficients[2]*t +fit$coefficients[3]*t^2,lwd = 2,col = "indianred")
legend("top",c("Deaths","Fitted values","Trend"), col=c("black","steelblue", "indianred"),lty = 1,lwd = 2, cex=1,bty = "n")
plot(index(deaths),fit$residuals, type = "n",ylab = "Residuals",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(fit$residuals,lwd = 2, col = "steelblue")
dev.off()

sig2hat <- sum(fit$residuals^2)/T
K <- T - fit$df.residual
IC <- c(log(sig2hat) + (T+2*K)/T, log(sig2hat) + (T+K)/(T-K-2),log(sig2hat) + K*log(T)/T)

# Trigonometric season
omega <- c(2*pi/6,2*pi/12)
cosTerm1 <- cos(omega[1]*t)
cosTerm2 <- cos(omega[2]*t)
sinTerm1 <- sin(omega[1]*t)
sinTerm2 <- sin(omega[2]*t)

summary(fit <- lm(deaths ~ t + I(t^2) + cosTerm1 + sinTerm1 + cosTerm2 + sinTerm2))

phi1 <- atan(-fit$coefficients["sinTerm1"]/fit$coefficients["cosTerm1"])
A1 <- sqrt(fit$coefficients["sinTerm1"]^2 + fit$coefficients["cosTerm1"]^2)

phi2 <- atan(-fit$coefficients["sinTerm2"]/fit$coefficients["cosTerm2"])
A2 <- sqrt(fit$coefficients["sinTerm2"]^2 + fit$coefficients["cosTerm2"]^2)

pdf(file=paste0(savehere,"deathEst.pdf"), width=8,height=5.1)   
par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(deaths, type = "n",ylab = "Deaths", ylim = c(6500,12500),xlab = "Year")
grid(lty=1, col = gray(.9))
lines(deaths,lwd = 2,type = "o")
lines(fit$fitted.values, col = "steelblue", lwd = 2)
lines(fit$fitted.values, col = "steelblue", lwd = 2)
lines(index(deaths),fit$coefficients[1] + fit$coefficients[2]*t +fit$coefficients[3]*t^2,lwd = 2,col = "indianred")
legend("top",c("Deaths","Fitted values","Trend"), col=c("black","steelblue", "indianred"),lty = 1,lwd = 2, cex=1,bty = "n")
plot(index(deaths),fit$residuals, type = "n",ylab = "Residuals",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(fit$residuals,lwd = 2, col = "steelblue")
dev.off()

sig2hat <- sum(fit$residuals^2)/T
K <- T - fit$df.residual
IC <- c(log(sig2hat) + (T+2*K)/T, log(sig2hat) + (T+K)/(T-K-2),log(sig2hat) + K*log(T)/T)

# Trigonometric functions

t <- seq(0,50,0.01)

pdf(file=paste0(savehere,"trigoPlot.pdf"), width=8,height=4)   
par(mfrow = c(1,1), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
Y1 <- 2*cos(2*pi*(1/12)*t + 0.5*pi)
plot(t,Y1,type = "n",ylab = "",xlab = "Time",main = bquote(~Y[t]~"="~2*cos(2*pi*(1/12)* t + 0.5*pi)))
grid(lty=1, col = gray(.9))
lines(t,Y1,type = "l", lwd = 2)
dev.off()


pdf(file=paste0(savehere,"trigoPlot1.pdf"), width=8,height=5.5)   

par(mfrow = c(2,1), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
Y1 <- 2*cos(2*pi*(1/12)*t + 0.5*pi)
X1 <- 10*cos(2*pi*(1/6)*t + 2*pi)
Z1 <- 6*cos(2*pi*(1/24)*t)

# plot(t,Y1,type = "n",ylab = "",xlab = "Time",main = bquote(bold(A~"*"~cos(2*pi(1/12))~"= 2")))
plot(t,Y1+X1+Z1,type = "n",ylab = "",xlab = "Time")
grid(lty=1, col = gray(.9))
lines(t,Y1,type = "l", lwd = 2)
lines(t,X1,type = "l", lwd = 2,col = "red")
lines(t,Z1,type = "l", lwd = 2,col = "blue")

plot(t,X1+Y1+Z1,type = "n",ylab = "",xlab = "Time")
grid(lty=1, col = gray(.9))
lines(t,Y1+X1+Z1,type = "l", lwd = 2)
dev.off()



#Elimininating trend using differences

T <- 250
t <- 1:T
Y <- 5 + 0.002*t + 5*t^2 + rnorm(T)*sqrt(5000000)

pdf(file=paste0(savehere,"qtrendElim.pdf"), width=8,height=6) 
par(mfrow = c(3,1), mar=c(2,2,1,1)+0.5, mgp=c(1.6,.6,0), cex.main=1.05)
plot(t,Y,type = "n",xlab = "Time",ylab = expression(Y[t]))
grid(lty=1, col = gray(.9))
lines(t,Y,lwd = 2,type = "o")
plot(t[-1],diff(Y),type = "n",xlab = "Time",ylab = expression(nabla*Y[t]))
grid(lty=1, col = gray(.9))
lines(t[-1],diff(Y),lwd = 2,type = "o")
plot(t[-1][-1],diff(diff(Y)),type = "n",xlab = "Time",ylab = expression(nabla*nabla*Y[t]))
grid(lty=1, col = gray(.9))
lines(t[-1][-1],diff(diff(Y)),lwd = 2,type = "o")
dev.off()

# Differencing

pdf(file=paste0(savehere,"deathDiff.pdf"), width=8,height=6)   
par(mfrow = c(3,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(deaths,type = "n",xlab = "Year",ylab = "Accidental deaths")
grid(lty=1, col = gray(.9))
lines(deaths,type = "o",lwd = 2)
plot(diff(deaths,12),type = "n", xlab = "Year",ylab = expression(nabla[12]))
grid(lty=1, col = gray(.9))
lines(diff(deaths,12),type = "o",lwd = 2)
plot(diff(diff(deaths,12)),type = "n",xlab = "Year",ylab = expression(nabla * nabla[12]))
grid(lty=1, col = gray(.9))
lines(diff(diff(deaths,12)),type = "o",lwd = 2)
dev.off()




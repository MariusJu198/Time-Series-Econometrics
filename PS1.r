#sets the seed
set.seed(1)

rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)


getSymbols("MRTSSM44X72USN",src = "FRED")
MR <- zoo(MRTSSM44X72USN$MRTSSM44X72USN, index(MRTSSM44X72USN))
Date <- as.yearmon(index(MR), "%Y %m")


Y=log(MR)
T=length(Y)
t=1:T
#this command gives you the seasonal cycles in the data that you have specified. In this example I use monthly data, so I get the number from 1:12
M=factor(cycle(Date))

#We know want to model 3 different models, with 3 different trends 

#I regress y on a timetrend and a 12 month seasonal dummy
M1=lm(Y~t+M)
M2=lm(Y~t+I(t^2)+M)
M3=lm(Y~t+I(t^2)+I(t^3)+M)


#Now I compute the sigma squared for each model. Sigma squared is calculated as Sum(squared residuals)/T

M1_sigma=sum(M1$residuals^2)/T
M2_sigma=sum(M2$residuals^2)/T
M3_sigma=sum(M3$residuals^2)/T

#Binding all of the sigma squared values into 1 vector
sig2=c(M1_sigma, M2_sigma, M3_sigma)

#Number of parameters of the 3 models

K=c(12,13,14)

#I stack all AIC values into a vector
AIC=log(sig2)+(T+2*K)/T
AICc=log(sig2)+(T+K)/(T-K-2)
BIC=log(sig2)+log(T)*K/T

information_criteria = rbind(AIC, AICc, BIC)





#From the information criterions we see that they all choose model 3 as the best model


############################################# Plot of fitted values ###########################################

#Initialising the graph layout
par(mfrow = c(4,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(Y, type = "n",xlab = "Year")
grid(lty=1, col = gray(.9))
legend("top",c("Log Sales","Fitted values","Trend"), col=c("black","steelblue", "indianred"),lty = 1,lwd = 2, cex=1,bty = "n")


#Plots the Y values
lines(index(Y),Y,lwd = 2,type = "o")

#Plots the fitted values
lines(M3$fitted.values, col = "steelblue", lwd = 2)
#Plots the trend. Notice we have omitted the seasonal coefficient 
lines(index(Y),M3$coefficients[1] + M3$coefficients[2]*t +M3$coefficients[3]*t^2+M3$coefficients[4]*t^3,lwd = 2,col = "indianred")

#########################################################################################


############################################# Plot of residuals  ###########################################

plot(index(Y),M3$residuals, type = "n",ylab = "Residuals",xlab = "Year")
grid(lty=1, col = gray(.9))
lines(M3$residuals,lwd = 2, col = "steelblue")

############################################# Residual diagnostic ###########################################

#Autocorrelation function of residuals
acf(as.numeric(M3$residuals))
qqnorm(as.numeric(M3$residuals))
qqline(as.numeric(M3$residuals), col = "steelblue")



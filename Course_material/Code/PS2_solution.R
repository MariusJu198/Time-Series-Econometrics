rm(list = ls())
library(astsa)
library(readxl)
library(forecast)
library(xts)
library(quantmod)

tp <- 42
data <- read_xlsx("C:\\Users\\au282988\\Dropbox\\Postdoc\\Teaching\\Time Series Econometrics E2022\\Data\\PS2.xlsx")
dates <- seq(as.Date("1980/1/1"), by = "month", length.out = 12*tp)
ts <- zoo(data$PS2,dates)
PS2 <- data$PS2

T <- 12*tp
t <- 1:T

M <- factor(rep(1:12,tp))

summary(trend <- lm(PS2 ~ t + I(t^2) + M ))
 
acf2(trend$residuals) # ACF and PACF

maxp <- 3
maxq <- 3
AIC <- matrix(NA,maxp+1,maxq+1)
BIC <- matrix(NA,maxp+1,maxq+1)

for (p in 0:maxp){
  for(q in 0:maxq){
    model <- arima(x=as.numeric(trend$residuals), order=c(p,0,q), method="CSS-ML")
    
    AIC[p+1,q+1] <- AIC(model); BIC[p+1,q+1] <- BIC(model)
  }
}
AIC # ARMA(1,1) is preferred
BIC # ARMA(1,1) is preferred

model <- arima(x=as.numeric(trend$residuals), order=c(1,0,1), method="CSS-ML") #Estimate preferred model

acf2(model$residuals) # Looks great!

#Prepare forecast study
inSample <- ts[index(ts)<="2019-12-01",]
outSample <- ts[index(ts)>"2019-12-01",]


# In-sample estimation
t <- 1:length(inSample)
tOut <- (length(inSample)+1):T
M <- factor(rep(1:12,length(inSample)/12))
summary(trend <- lm(inSample ~ t + I(t^2) + M))

model <- arima(as.numeric(trend$residuals), order=c(1,0,1), method="CSS-ML")

#Forecating the stationary part using the truncated predictor (see p. 109 in SS and Example 3.24)
epsi <- matrix(NA,length(inSample),1)
x <- as.numeric(trend$residuals)
for (i in 1:length(inSample)){
  if (i==1){
    epsi[i] <- x[i]
  }else{
    epsi[i] <- x[i] - model$coef["ar1"]*x[i-1] - model$coef["ma1"]*epsi[i-1] 
  }
}

forec <- matrix(NA,24,1)

c <- 1
for (f in (length(inSample)+1):T){
  if (c==1){
    forec[c] <- model$coef["ar1"]*x[f-1] + model$coef["ma1"]*epsi[f-1]
  }else{
    forec[c] <- model$coef["ar1"]*forec[c-1] 
  }
  c <- c + 1
}


# Adding the forecast of the ARMA part with the trend and seasonal component

ARMA_part <- ts(forec,start = c(2020,1,1),frequency = 12)
trend_part <- trend$coefficients[1] + tOut*trend$coefficients[2] + tOut^2*trend$coefficients[3]
seasonal_part <- rep(c(0,trend$coefficients[4:length(trend$coefficients)]),2)
total_forecast <- trend_part + seasonal_part + ARMA_part

par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
ts.plot(total_forecast, as.numeric(outSample),ylim = c(350,450), col = c("steelblue","indianred"), lwd = 2)
grid()
legend("topleft" ,c("Forecast","Actual values"), col=c("steelblue","indianred"),lty=c(1,1),lwd = 2, cex=1,box.lty=1)

combined <- zoo(c(as.numeric(inSample),as.numeric(total_forecast)),dates)

plot(combined, col = "steelblue",lwd = 2, ylab = "")
lines(ts,col = "indianred",lwd = 2)
grid()
legend("topleft" ,c("Forecast","Actual values"), col=c("steelblue","indianred"),lty=c(1,1),lwd = 2, cex=1,box.lty=1)





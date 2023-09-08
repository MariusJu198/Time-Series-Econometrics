rm(list = ls())
library(astsa)
library(xts)
library(quantmod)
library(readxl)
library(forecast)
library(rugarch)
library("writexl")
set.seed(1)
tp <- 42

dates <- seq(as.Date("1980/1/1"), by = "month", length.out = 12*tp)

T <- 12*tp
t <- 1:T

mu <- 300 - 0.03*t + 0.0005*t^2

df <- data.frame(x = factor(rep(1:12,tp)))
df <- model.matrix(~.,data = df)[,-1]

ss <- sqrt(10)*rnorm(11,0,1)
ss <- abs(ss[order(ss)])
ss <- ss - mean(ss)
gamma <- rowSums(as.matrix(sweep(df, MARGIN=2,ss, `*`)))

w <- 3*rnorm(T)
X <- arima.sim(model = list(ar = 0.95,ma = 0.5,order = c(1,0,1)), n = T,innov = w)
par(mfrow = c(5,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
plot(t,mu,type = "l")
plot(t,gamma,type = "l")
plot(t,X,type = "l")
PS2 <- mu+gamma+X

write_xlsx(data.frame(PS2,dates),"C:\\Users\\au282988\\Dropbox\\Postdoc\\Teaching\\Time Series Econometrics E2022\\Data\\PS2.xlsx")
ts <- zoo(PS2,dates)
plot(ts)

M <- factor(rep(1:12,tp))

summary(trend <- lm(PS2 ~ t + I(t^2) + M ))

acf2(trend$residuals)

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
AIC
BIC

model <- arima(x=as.numeric(trend$residuals), order=c(1,0,1), method="CSS-ML")

acf2(model$residuals)

inSample <- ts[index(ts)<="2019-12-01",]
outSample <- ts[index(ts)>"2019-12-01",]

t <- 1:length(inSample)
tOut <- (length(inSample)+1):T
M <- factor(rep(1:12,length(inSample)/12))
summary(trend <- lm(inSample ~ t + I(t^2) + M))

model <- arima(as.numeric(trend$residuals), order=c(1,0,1), method="CSS-ML")


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

par(mfrow = c(2,1), mar=c(2,2,0,0)+0.75, mgp=c(1.6,.6,0), cex.main=1.2)
ts.plot(ts(forec,start = 481,end = 504,frequency = 1) + trend$coefficients[1] + tOut*trend$coefficients[2] + tOut^2*trend$coefficients[3] + rep(c(0,trend$coefficients[4:length(trend$coefficients)]),2)
        , as.numeric(outSample),ylim = c(350,450), col = c("steelblue","indianred"), lwd = 2)
grid()

pred <- predict(model,24)


ts.plot(pred$pred + trend$coefficients[1] + tOut*trend$coefficients[2] + tOut^2*trend$coefficients[3] + rep(c(0,trend$coefficients[4:length(trend$coefficients)]),2)
, as.numeric(outSample),ylim = c(350,450), col = c("steelblue","indianred"), lwd = 2)
grid()



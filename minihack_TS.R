library(forecast)
library(GeneCycle)
library(fpp)
library(forecast)
setwd("E:\\SS\\AV\\OnlineHack\\Mini Hack")

train <- read.csv("Train_JPXjxg6.csv")
test <- read.csv("Test_mvj827l.csv")

head(train)

## time series
tr <- ts(train[,2] , frequency = 24)

plot(tr)

# Decompose
f <- decompose (tr) 
# seasonal figures 
f$figure 
plot( f , type = "b" , xaxt = "n" , xlab = " " ) 


# Exponential Smoothing
model <- HoltWinters(tr)
plot(tr)
lines(model$fitted[,"xhat"], col="red")

# To predict the next values in the time series:

pred <- predict(model, n.ahead=5112) # values for the next 7 months
plot(tr)
lines(pred, col="red", lty=2)

### preparing submission
sub <- data.frame(Datetime = test$Datetime , Count = pred)
names(sub) <- c("Datetime", "Count")
write.csv(sub, "sub.csv",row.names=FALSE)
## LB 202.604195392


trlog <- log(tr)


## checking stationarity of series
adf.test(diff(trlog), alternative="stationary", k=0)
Box.test(trlog, lag=120)


acf(diff(log(tr)))    
pacf(diff(log(tr)))   
 
plot(trlog)

# other tests
library(fpp)
adf.test(tr, alternative="stationary") # small p-values suggest the data is stationary
kpss.test(tr.dif) # small p-values suggest that the series is not stationary and a differencing is required

### checking p, q component ( d=2, ma=2)

acf(tr.dif,  lag.max=120)
acf(tr.dif,  lag.max=20, plot=FALSE)
pacf(tr.dif, lag.max=120)
pacf(tr.dif, lag.max=20, plot=FALSE)



library(forecast)

# Automated forecasting using an ARIMA model
fit1 <- auto.arima(trlog) 

# fit an ARIMA model of order P, D, Q
fita1 <-arima(trlog, order=c(1, 1, 1))

# predictive accuracy
library(forecast)
accuracy(fita1)

# predict next observations
library(forecast)
pred2 <- forecast(fita1, 5112)
plot(forecast(fita1, 5112))
pr2 <- pred2$mean

pred <- predict(fita1 , n.ahead = 5112)
prexp <- exp(pred$pred)

### preparing submission
sub1 <- data.frame(Datetime = test$Datetime , Count = prexp)
names(sub1) <- c("Datetime", "Count")
write.csv(sub1, "sub1.csv",row.names=FALSE)




























































































































































































































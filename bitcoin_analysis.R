# ==============================
# 0. metrics prepare
# ==============================
library(fpp2) #for the data
library(urca)#for the KPSS test
library(tseries)#for ARIMA simulation
library(forecast)# for ACF and PACF function
library(lmtest)# test significance of est
library(Metrics)
library(quantmod)
# ==============================
# 1. Data Acquisition and Visualization
# ==============================
data = getSymbols("BTC-USD",src= "yahoo",
                  from="2020-01-01", to ="2025-12-31",
                  interval="ld",auto.assign=F)
head(data)

#Identify the patterns in the data
close = Cl(data)
close

autoplot(close)

dev.new()
autoplot(close[1:730])# The close price shows an increasing trend

# ==============================
# 2. Stationarity Test and Determination of Difference Order
# ==============================

summary(ur.kpss(close))
#the KPSS test statistic is 15.5887> critical level(5%)=0.463: rejiect H0: the close price is not stationary

dclose= diff(close)
summary(ur.kpss(dclose))
#the differenciated close price is stationary because KPSS test stastic=0.1634 < critival value(5%)
# the order of integration of the close price is 1 

# ==============================
# 3. Model Selection
# ==============================
#split the data
n = length(close)
h = n-floor(0.75*n)

train_end=time(close)[n-h]
test_start=time(close)[n-h+1]

training=window(close, end=train_end)
test = window(close, start=test_start)

#fit the best ARIMA model to the data
fit the best ARIMA model to the data
fit = auto.arima(training)
summary(fit)

# test the significance of estimated parameters
coeftest(fit)
# prob(mal) = 0.01428< 0.05:reject H0:mal is significant at the 5% level

# ==============================
# 4. Predicted Data Classification
# ==============================
pred = forecast(fit,h=h)
autoplot(pred)

# ==============================
# 5. Model Evaluation
# ==============================
rmse_arima = rmse(as.numeric(test),as.numeric(pred$mean))
rmse_arima

mae_arima=mae(as.numeric(test),as.numeric(pred$mean))  
mae_arima

# ==============================
# 6. Comparison with Holt-Winters Model
# ==============================
# Additive Holt-Winters
pred_add = hw(ts(training,frequency=7),h=h,seasonal="additive")
rmse_add = rmse(as.numeric(test),as.numeric(pred_add$mean))
rmse_add
mae_add=mae(as.numeric(test),as.numeric(pred_add$mean))  
mae_add
autoplot(pred_add)

# Multiplicative Holt-Winters
pred_mult = hw(ts(training,frequency=7),h=h,seasonal="multiplicative")
rmse_mult = rmse(as.numeric(test),as.numeric(pred_mult$mean))
rmse_mult
mae_mult=mae(as.numeric(test),as.numeric(pred_mult$mean))  
mae_mult
autoplot(pred_mult)

#Output comparison results
comp = data.frame(Method = c("ARIMA","ADD_HW","MULT_HW"),
                  RMSE=c(rmse_arima,rmse_add,rmse_mult),
                  mae=c(mae_arima,mae_add,mae_mult))
comp
comp[c(which.min(comp$RMSE),which.min(comp$MAE)),1]

#Optimal Model
pred_mult$model
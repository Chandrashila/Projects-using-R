#Installing required R libraries
install.packages("tidyverse")
library(readr)
install.packages("forecast")
library(forecast)
install.packages("fpp2")
library(fpp2)
library(TTR)
library(dplyr)
library(ggplot2)
library(readxl)
#Reading data from excel file
CESC_MMYYYY_data_edit <-read_excel("C:/Users/rumic/Desktop/Desktop/CESC/Cesc_data/CESC_MMYYYY_data_edit.xlsx",
col_types = c("date", "numeric", "text"))
View(CESC_MMYYYY_data_edit)
cesc_mp = CESC_MMYYYY_data_edit
#Checking if import of data is correct
glimpse(cesc_mp)
#Splitting the data into Training and Test
cesc_mp_train = subset(cesc_mp, Class == 'Train')
cesc_mp_test = subset(cesc_mp, Class == 'Test')
nrow(cesc_mp_train);nrow(cesc_mp_test)
cesc_mp_ts = ts(cesc_mp_train[,2], start=c(2014,1), end = c(2018,12), frequency = 12)
cesc_mp_ts_test = ts(cesc_mp_test[,2], start=c(2019,1), end = c(2019,12), frequency = 12)
#Plotting original time series
plot(cesc_mp_ts)
#Creating a function to calculate MAPE errors of models
mape = function(actual, pred){mape =
mean(abs((actual - pred)/actual))*100
return (mape)}
#Seasonality test
install.packages("seastests")
library(seastests)
isSeasonal(cesc_mp_ts)
summary(wo(cesc_mp_ts))
#Thus, original time series is not seasonal
#Stationarity test:
#Null hypothesis: Time series is not stationary
#Alternative: Time series is stationary
#ADF test for stationarity
install.packages("tseries")
library(tseries)
adf.test(cesc_mp_ts)
install.packages("tseries")
#Differencing as original time series is not stationary
cesc_mp_ts_count_d1 = diff(cesc_mp_ts, differences = 1)
plot(cesc_mp_ts_count_d1)
#ADF test for stationarity after first differencing
adf.test(cesc_mp_ts_count_d1, alternative = "stationary")
library(tseries)
#ADF test for stationarity after first differencing
adf.test(cesc_mp_ts_count_d1, alternative = "stationary")
#As p-value < 0.05, thus we reject the null hypothesis. Hence, first differencing time series is stationary. Hence, d = 1 for ARIMA model
#White noise test for original time series
Box.test(cesc_mp_ts, lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#As P-value is very small and less than 0.05, we reject the null hypothesis (Series is white noise). Thus, original time series is no white noise.
#Another method to test white noise
install.packages("normwhn.test")
library(normwhn.test)
whitenoise.test(cesc_mp_ts)
#As test value is very less than 0.05, hence we again reject the null hypothesis. Series has no white noise.
#Plotting ACF, PACF and IACF to check seasonality, trend in original time series and to understand the order of ARIMA model (by checking significant peaks)
#ACF and PACF on original time series (before first differencing and Lag = 12)
acf(cesc_mp_ts,lag.max = 12,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
#As we can see from ACF plot that 4 peaks are significant. Thus, y(t) for Manpower forecast depends on y(t-1), y(t-2) and y(t-3) values and q for MA(q) model <= 3
pacf(cesc_mp_ts, lag.max = 12)
# We can see that only first peak is significant. Thus, AR(p), p<=1
#ACF and PACF for first differencing
#ACF with Lag = 24
acf(cesc_mp_ts_count_d1,lag.max = 24,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
#ACF with Lag = 100
acf(cesc_mp_ts_count_d1,lag.max = 100,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
#None of the peaks are significant
#PACF with Lag = 24
pacf(cesc_mp_ts_count_d1, lag.max = 24)
#PACF with Lag = 100
pacf(cesc_mp_ts_count_d1, lag.max = 100)
#On observing PACF plot we can say 8th lag is significant for first differencing
#ARIMA Models
#Fitting ARIMA model of order (8,1,0)
cesc_fitARIMA1 = arima(cesc_mp_ts, order=c(8,1,0),method="ML")
print(cesc_fitARIMA1)
#ARIMA (8,1,0) performance metrics
library(lmtest)
coeftest(cesc_fitARIMA1)
#Fit ARIMA (2,1,0) on training set
cesc_fitARIMA2 = arima(cesc_mp_ts, order=c(2,1,0),method="ML")
print(cesc_fitARIMA2)
#ARIMA (2,1,0) performance metrics
coeftest(cesc_fitARIMA2)
#ARIMA (2,1,0) performance metrics
library(fpp)
accuracy(cesc_fitARIMA2)
#White Noise test for residuals
Box.test(cesc_fitARIMA2$residuals, lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#As P-value > 0.05 , we fail to reject the null hypothesis. Thus, residuals resemble white noise.
#ACF of residuals
acf(cesc_fitARIMA2$residuals)
#ACF has no significant correlations.
#Plotting residuals
qqnorm(cesc_fitARIMA2$residuals)
qqline(cesc_fitARIMA2$residuals)
#From the results, it looks like the standardized residuals are close to following a normal distribution #except few outliers, so there is no concern about satisfying the normality assumption in the model.
plot(cesc_fitARIMA2$residuals)
#Also, residuals have mean approximately equal to zero and constant variance except between 2016 #and 2017.
#Predicting using ARIMA (2,1,0)
cesc_arima_forecast2= predict(cesc_fitARIMA2,12)
cesc_arima_forecast2$pred
mape(cesc_mp_test$ManPower, cesc_arima_forecast2$pred)
#MAPE error for prediction is 39%, which is very bad prediction accuracy.
#Fitting ARIMA model of order (1,1,0)
cesc_fitARIMA3 = arima(cesc_mp_ts, order=c(1,1,0),method="ML")
print(cesc_fitARIMA3)
#ARIMA (1,1,0) performance metrics
coeftest(cesc_fitARIMA3)
#MAPE error is 9%, which is much better than the rest of the models.
#White Noise test for residuals
Box.test(cesc_fitARIMA3$residuals, lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#As P-value > 0.05 , we fail to reject the null hypothesis. Thus, residuals resemble white noise.
#ACF of residuals
acf(cesc_fitARIMA3$residuals)
#ACF has no significant correlations.
#Plotting residuals
qqnorm(cesc_fitARIMA3$residuals)
qqline(cesc_fitARIMA3$residuals)
#From the results, it looks like the standardized residuals are close to following a normal distribution #except few outliers, so there is no concern about satisfying the normality assumption in the model.
plot(cesc_fitARIMA3$residuals)
#Also, residuals have mean approximately equal to zero and constant variance except one spike.
#Predicting using ARIMA (1,1,0)
cesc_arima_forecast3= predict(cesc_fitARIMA3,12)
cesc_arima_forecast3$pred
mape(cesc_mp_test$ManPower, cesc_arima_forecast3$pred)
#MAPE error for prediction is 38%, which is very bad prediction accuracy.
#ARIMA model (with auto.arima)
cesc_arima_model = auto.arima(cesc_mp_ts)
summary(cesc_arima_model)
#MAPE error is 11%, which is much better than the rest of the models but more than ARIMA (1,1,0).
#White Noise test for residuals
Box.test(cesc_arima_model$residuals, lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#As P-value > 0.05 , we fail to reject the null hypothesis. Thus, residuals resemble white noise.
#ACF of residuals
acf(cesc_arima_model$residuals)
#ACF has no significant correlations.
#Plotting residuals
qqnorm(cesc_fitARIMA2$residuals)
qqline(cesc_fitARIMA2$residuals)
#From the results, it looks like the standardized residuals are close to following a normal distribution #except few outliers, so there is no concern about satisfying the normality assumption in the model. #But there is no trend in the residuals.
#Also, residuals have mean approximately equal to zero and constant variance except one spike.
#Predicting using ARIMA (1,0,0) - for Test set and year 2020
cesc_autoarima_forecast= predict(cesc_arima_model,24)
cesc_autoarima_forecast$pred
mape(cesc_mp_test$ManPower, cesc_autoarima_forecast$pred)
#Also, residuals have mean approximately equal to zero and constant variance except one spike.
#Predicting using ARIMA (1,0,0) - for Test set and year 2020
cesc_autoarima_forecast= predict(cesc_arima_model,24)
View(cesc_autoarima_forecast$pred)
mape(cesc_mp_test$ManPower, cesc_autoarima_forecast$pred)
#MAPE error for prediction is 12% when predicting for a year but the error rises to 14% when predicting for 2 years. However, this is far better than other ARIMA models. So, we choose ARIMA(1,0,0) for prediction.
#Creating prediction dataframe using auto ARIMA (1,0,0) model as MAPE on training set is 11% and MAPE for prediction accuracy is 12%, which is better than other models.
cesc_mp_final = rbind(cesc_mp[61:72,], c("2020-01-01",".","New"), c("2020-02-01",".","New"), c("2020-03-01",".","New"), c("2020-04-01",".","New"), c("2020-05-01",".","New"), c("2020-06-01",".","New"), c("2020-07-01",".","New"), c("2020-08-01",".","New"), c("2020-09-01",".","New"), c("2020-10-01",".","New"), c("2020-11-01",".","New"), c("2020-12-01",".","New"))
cesc_mp_final=as.data.frame(cesc_mp_final)
autoarima_fr=data.frame("Predicted Value" =cesc_autoarima_forecast$pred)
cesc_mp_final = cbind(cesc_mp_final, 'Forecast' = autoarima_fr)
View(cesc_mp_final)

#Creating model using Facebook Prophet
install.packages("prophet")
library("prophet")
install.packages("data.table")
library(data.table)
install.packages("dplyr")
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library("ggplot2")
#Visualizing training data
ggplot(cesc_mp_train)+geom_line(aes(cesc_mp_train$MMMYYYY,cesc_mp_train$ManPower))
#Change column names to 'y' and 'ds' as that's a requirement for using Prophet
names(cesc_mp_train) = c("ds", "y")
View(cesc_mp_train)
cesc_mp_test = subset(cesc_mp, Class == 'Test')
View(cesc_mp_train)
cesc_mp_train = subset(cesc_mp, Class == 'Train')
cesc_mp_train_fp = cesc_mp_train
View(cesc_mp_train_fp)
#Change column names to 'y' and 'ds' as that's a requirement for using Prophet
names(cesc_mp_train_fp) = c("ds", "y")
cesc_mp_train_fp = cesc_mp_train_fp[-1]
cesc_mp_train_fp = cesc_mp_train
names(cesc_mp_train_fp) = c("ds", "y")
cesc_mp_train_fp = cesc_mp_train_fp[-3]
#Model Building
m = prophet(cesc_mp_train_fp)
m = prophet::prophet(cesc_mp_train_fp)
library(prophet)
m = prophet::prophet(cesc_mp_train_fp)
m = prophet::prophet(cesc_mp_train_fp)
m = prophet::prophet(cesc_mp_train_fp)
#Creating model using Facebook Prophet
install.packages("prophet")
library("prophet")
install.packages("data.table")
install.packages("prophet")
library("prophet")
install.packages("data.table")
library(data.table)
install.packages("dplyr")
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library("ggplot2")
View(cesc_mp_train_fp)
library(prophet)
m = prophet::prophet(cesc_mp_train_fp)
cesc_prophet_future = make_future_dataframe(m, periods = 365)
cesc_prophet_forecast = predict(m, cesc_prophet_future)
# Visualize forecast
plot(m, cesc_prophet_forecast)
View(cesc_prophet_forecast)
tail(cesc_prophet_forecast[c('ds','yhat','yhat_lower','yhat_upper')])
cesc_mp_prophet_pred = tail(cesc_prophet_forecast[c('ds','yhat','yhat_lower','yhat_upper')])
View(cesc_mp_prophet_pred)
#Plotting trend, weekly seasonality and yearly seasonality
prophet_plot_components(m, cesc_prophet_forecast)
#Interactive plot
dyplot.prophet(m, cesc_prophet_forecast)
cesc_prophet_future = make_future_dataframe(m, periods, freq = "month")
cesc_prophet_future = make_future_dataframe(m, periods=12, freq = "month")
cesc_prophet_forecast = predict(m, cesc_prophet_future)
plot(m, cesc_prophet_forecast)
cesc_mp_prophet_pred = tail(cesc_prophet_forecast[c('ds','yhat','yhat_lower','yhat_upper')])
View(cesc_mp_prophet_pred)
cesc_prophet_future = make_future_dataframe(m, periods=24, freq = "month")
cesc_prophet_forecast = predict(m, cesc_prophet_future)
plot(m, cesc_prophet_forecast)
cesc_mp_prophet_pred = tail(cesc_prophet_forecast[c('ds','yhat','yhat_lower','yhat_upper')])
View(cesc_mp_prophet_pred)
View(cesc_mp_prophet_future)
View(cesc_prophet_future)
View(cesc_prophet_forecast)
cesc_mp_prophet_pred = cesc_prophet_forecast[c('ds','yhat','yhat_lower','yhat_upper')]
View(cesc_mp_prophet_pred)
#Building the model on whole dataset
m = prophet::prophet(cesc_mp_ts)
View(cesc_mp)
cesc_mp_fp=cesc_mp
names(cesc_mp_fp) = c("ds", "y")
cesc_mp_fp=cesc_mp
cesc_mp_fp = cesc_mp_fp[-3]
names(cesc_mp_fp) = c("ds", "y")
View(cesc_mp_fp)
#Model Building
m1 = prophet::prophet(cesc_mp_fp)
cesc_prophet_future1 = make_future_dataframe(m1, periods=24, freq = "month")
cesc_prophet_forecast1 = predict(m1, cesc_prophet_future1)
plot(m1, cesc_prophet_forecast1)
cesc_mp_prophet_pred1 = cesc_prophet_forecast1[c('ds','yhat','yhat_lower','yhat_upper')]
View(cesc_mp_prophet_pred1)
#Plotting trend, weekly seasonality and yearly seasonality
prophet_plot_components(m, cesc_prophet_forecast1)
#Interactive plot
dyplot.prophet(m1, cesc_prophet_forecast1)
merge_fp = merge(x=cesc_mp_fp,y=cesc_mp_prophet_pred1,by="ds")
View(merge_fp)
residuals_fp=merge_fp$y - merge_fp$yhat
#plot the residuals
merge_fp$resid=merge_fp$y - merge_fp$yhat
ggplot(merge_fp,aes(ds,resid)) +
geom_point() + geom_smooth() +
ggtitle("Prophet",
subtitle = paste0("RMSE: ",round(sqrt(mean(merge_fp$resid^2)),2)))
#Whitenoise test
Box.test(merge_fp$resid, lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#performance metrics
cesc_prophet_perf=performance_metrics(cesc_prophet_future1)
#cross validation
cesc_fp_cv <- cross_validation(m1, initial = 60, period = 12, horizon = 12, units = 'months')
cesc_fp_cv <- cross_validation(m1, initial = 1825, period = 180, horizon = 365, units = 'days')
cesc_fp_cv <- cross_validation(m1, initial = 1525, period = 180, horizon = 365, units = 'days')
cesc_prophet_perf=performance_metrics(cesc_fp_cv)
head(cesc_prophet_perf)
plot_cross_validation_metric(cesc_fp_cv, metric = 'mape')
prophet_plot_components(m1, cesc_prophet_forecast1)
dyplot.prophet(m1, cesc_prophet_forecast1)
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

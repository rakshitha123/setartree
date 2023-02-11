if (!('forecast' %in% rownames(installed.packages())))
  install.packages("forecast")

library(setartree)
library(forecast) # The 'forecast' package is only required for plotting examples shown in lines 17-30.


### Case 1 - When need to obtain forecasts for a list of time series ###

# Training SETAR-Forest
forest1 <- setarforest(chaotic_logistic_series, bagging_freq = 3)

# Obtaining predictions
forest_forecasts <- forecast(forest1, chaotic_logistic_series)

# Plotting - can use either plot or autoplot functions
plot(forest_forecasts) # Plot all time series
autoplot(forest_forecasts)

plot(forest_forecasts$forecast$T1) # Plot the 1st time series
autoplot(forest_forecasts$forecast$T1)

plot(forest_forecasts$forecast$T10) # Plot the 10th time series
autoplot(forest_forecasts$forecast$T10)

custom_forecasts <- forest_forecasts # Plot a selected set of time series - eg: 1st and 10th time series
custom_forecasts$forecast <- custom_forecasts$forecast[c(1, 10)]
custom_forecasts$method <- custom_forecasts$method[c(1, 10)]
plot(custom_forecasts)
autoplot(custom_forecasts)




### Case 2 - When need to obtain predictions for a dataframe/matrix containing model inputs including past time series lags and numerical/categorical covariates ###

# Training SETAR-Forest
forest2 <- setarforest(data = web_traffic_train[-1],
                       label = web_traffic_train[,1],
                       bagging_freq = 3,
                       stopping_criteria = "error_imp",
                       categorical_covariates = c("Project", "Access", "Agent"))

# Obtaining predictions
forecast(forest2, web_traffic_test)



if (!('forecast' %in% rownames(installed.packages())))
  install.packages("forecast")

library(setartree)
library(forecast) # The 'forecast' package is only required for plotting in line 17 and 18.


### Case 1 - When need to obtain forecasts for a list of time series ###

# Training SETAR-Forest
forest1 <- setarforest(chaotic_logistic_series, bagging_freq = 3)

# Obtaining predictions
forest_forecasts <- setartree:::forecast(forest1, chaotic_logistic_series)

# Plotting
plot(forest_forecasts[[1]]) # Plotting the 1st time series
plot(forest_forecasts[[10]]) # Plotting the 10th time series




### Case 2 - When need to obtain predictions for a dataframe/matrix containing model inputs including past time series lags and numerical/categorical covariates ###

# Training SETAR-Forest
forest2 <- setarforest(data = web_traffic_train[-1],
                       label = web_traffic_train[,1],
                       bagging_freq = 3,
                       stopping_criteria = "error_imp",
                       categorical_covariates = c("Project", "Access", "Agent"))

# Obtaining predictions
setartree:::forecast(forest2, web_traffic_test)



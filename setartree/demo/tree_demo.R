library(setartree)


### Case 1 - When need to obtain forecasts for a list of time series ###

# Training SETAR-Tree
tree1 <- setartree(chaotic_logistic_series)

# Obtaining predictions
tree_forecasts <- forecast(tree1, chaotic_logistic_series)

# Plotting - can use either plot or autoplot functions in the 'forecast' package
tryCatch({
  if(!is.null(find.package("forecast"))){
    library(forecast) # The 'forecast' package is only required for plotting examples shown in lines 17-30.

    plot(tree_forecasts) # Plot all time series
    autoplot(tree_forecasts)

    plot(tree_forecasts$forecast$T1) # Plot the 1st time series
    autoplot(tree_forecasts$forecast$T1)

    plot(tree_forecasts$forecast$T10) # Plot the 10th time series
    autoplot(tree_forecasts$forecast$T10)

    custom_forecasts <- tree_forecasts # Plot a selected set of time series - eg: 1st and 10th time series
    custom_forecasts$forecast <- custom_forecasts$forecast[c(1, 10)]
    custom_forecasts$method <- custom_forecasts$method[c(1, 10)]
    plot(custom_forecasts)
    autoplot(custom_forecasts)
  }
},error = function(e) {})



### Case 2 - When need to obtain predictions for a dataframe/matrix containing model inputs including past time series lags and numerical/categorical covariates ###

# Training SETAR-Tree
tree2 <- setartree(data = web_traffic_train[-1],
                   label = web_traffic_train[,1],
                   stopping_criteria = "both",
                   categorical_covariates = "Project")

# Obtaining predictions
forecast(tree2, web_traffic_test)


library(setartree)

### Case 1 - When need to obtain forecasts for a list of time series ###

# Training SETAR-Forest
forest1 <- setarforest(train_series, bagging_freq = 3)

# Obtaining predictions
forest_forecasts <- forecast(forest1, train_series)

# Plotting
plot(forest_forecasts[[1]]) # Plotting the 1st time series
plot(forest_forecasts[[10]]) # Plotting the 10th time series




### Case 2 - When need to obtain predictions for a dataframe/matrix containing model inputs including past time series lags, numerical and categorical covariates ###

# Training SETAR-Forest
forest2 <- setarforest(data = train_df[-1],
                       label = train_df[,1],
                       bagging_freq = 3,
                       stopping_criteria = "error_imp",
                       categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"))

# Obtaining predictions
forecast(forest2, test_df)



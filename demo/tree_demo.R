library(setartree)

### Case 1 - When need to obtain forecasts for a list of time series ###

# Training SETAR-Tree
tree1 <- setartree(train_series)

# Obtaining predictions
tree_forecasts <- forecast(tree1, train_series)

# Plotting
plot(tree_forecasts[[1]]) # Plotting the 1st time series
plot(tree_forecasts[[10]]) # Plotting the 10th time series




### Case 2 - When need to obtain predictions for a dataframe/matrix containing model inputs including past time series lags, numerical and categorical covariates ###

# Training SETAR-Tree
tree2 <- setartree(data = train_df[-1],
                   label = train_df[,1],
                   stopping_criteria = "error_imp",
                   categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"))

# Obtaining predictions
forecast(tree2, test_df)





#' Fitting SETAR-Forest models
#'
#' Fits a SETAR-Forest model either using a list of time series or an embedded input matrix and labels.
#'
#' @param data A list of time series (each list element is a separate time series) or a dataframe/matrix containing model inputs (the columns can contain past time series lags and/or external numerical/categorical covariates).
#' @param label A vector of true outputs. This parameter is only required when 'data' is a dataframe/matrix containing the model inputs.
#' @param lag The number of past time series lags that should be used when fitting each SETAR-Tree in the forest. This parameter is only required when 'data' is a list of time series. Default value is 10.
#' @param bagging_fraction The percentage of instances that should be used to train each SETAR-Tree in the forest.  Default value is 0.8.
#' @param bagging_freq The number of SETAR-Trees in the forest.  Default value is 10.
#' @param random_tree_significance Whether a random significance should be considered for splitting per each tree. Each node split within the tree considers the same significance level. When this parameter is set to TRUE, the "significance" parameter will be ignored. Default value is TRUE.
#' @param random_tree_significance_divider Whether a random significance divider should be considered for splitting per each tree. When this parameter is set to TRUE, the "significance_divider" parameter will be ignored. Default value is TRUE.
#' @param random_tree_error_threshold Whether a random error threshold should be considered for splitting per each tree. Each node split within the tree considers the same error threshold. When this parameter is set to TRUE, the "error_threshold" parameter will be ignored. Default value is TRUE.
#' @param depth  Maximum depth of each SETAR-Tree in the forest. Default value is 1000. Thus, unless specify a lower value, the depth of a SETAR-Tree is actually controlled by the stopping criterion.
#' @param significance In each SETAR-Tree in the forest, the initial significance used by the linearity test (alpha_0). Default value is 0.05.
#' @param significance_divider In each SETAR-Tree in the forest, the corresponding significance in a tree level is divided by this value. Default value is 2.
#' @param error_threshold In each SETAR-Tree in the forest, the minimum error reduction percentage between parent and child nodes to make a split. Default value is 0.03.
#' @param stopping_criteria The required stopping criteria for each SETAR-Tree in the forest: linearity test (lin_test), error reduction percentage (error_imp) or linearity test and error reduction percentage (both). Default value is 'both'.
#' @param verbose Controls the level of the verbosity of SETAR-Forest: 0 (errors/warnings), 1 (limited amount of information including the depth of the currently processing tree), 2 (full training information including the depth of the currently processing tree and stopping criterion related details in each tree). Default value is 2.
#' @param categorical_covariates Names of the categorical covariates in the input data. This parameter is only required when 'data' is a dataframe/matrix and it contains categorical variables.
#'
#' @return An object of class 'setarforest' which contains the following properties.
#' \item{trees}{A list of objects of class 'setartree' which represents the trained SETAR-Tree models in the forest.}
#' \item{lag}{The number of features used to train each SEATR-Tree in the forest.}
#' \item{feature_names}{Names of the input features.}
#' \item{coefficients}{Names of the coefficients of leaf node regresion models in each SETAR-Tree in the forest.}
#' \item{categorical_covariate_values}{Information about the categorical covarites used during training (only if applicable).}
#' \item{input_type}{Type of input data used to train the SETAR-Forest. This is 'list' if 'data' is a list of time series, and 'df' if 'data' is a dataframe/matrix containing model inputs.}
#' \item{execution_time}{Execution time of SETAR-Forest.}
#'
#' @examples
#' # Training SETAR-Forest with a list of time series
#' setarforest(chaotic_logistic_series, bagging_freq = 3)
#'
#' # Training SETAR-Forest with a dataframe containing model inputs where the model inputs may contain
#' # past time series lags and numerical/categorical covariates
#' setarforest(data = web_traffic_train[,-1],
#'             label = web_traffic_train[,1],
#'             bagging_freq = 3,
#'             stopping_criteria = "error_imp",
#'             categorical_covariates = c("Project", "Access", "Agent"))
#'
#' @export
setarforest <- function(data, label = NULL, lag = 10, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", verbose = 2, categorical_covariates = NULL){

  if(random_tree_significance & (verbose == 1 | verbose == 2))
    print("'random_tree_significance' = TRUE ... Ignored 'significance' parameter.")
  if(random_tree_error_threshold & (verbose == 1 | verbose == 2))
    print("'random_tree_error_threshold' = TRUE ... Ignored 'error_threshold' parameter.")
  if(random_tree_significance_divider & (verbose == 1 | verbose == 2))
    print("'random_tree_significance_divider' = TRUE ... Ignored 'significance_divider' parameter.")

  if(class(data) == "list"){
    if(length(data) < 1)
      stop("'data' should contain at least one time series.")
    if(!is.null(label)){
      print("'data' is a list of time series. 'label' is ignored.")
    }
    if(!is.null(categorical_covariates)){
      print("'data' is a list of time series. 'categorical_covariates' is ignored.")
    }
    fit.setarforest.series(data, lag, bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, verbose)
  }else if(class(data) == "data.frame" | "matrix" %in% class(data)){
    if(is.null(label))
      stop("'label' is missing. Please provide the true outputs corresponding with each instance in 'data'.")
    fit.setarforest.df(data, label, bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, "df", verbose, categorical_covariates)
  }else{
    stop("'data' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


#' Forecast method for SETAR-Forest fits
#'
#' Obtains forecasts for a given set of time series or a dataframe/matrix of new instances from a fitted SETAR-Forest model.
#'
#' @param forest An object of class 'setarforest' which is a trained SETAR-Forest model.
#' @param newdata A list of time series which need forecasts or a dataframe/matrix of new instances which need predictions.
#' @param h The required number of forecasts (forecast horizon). This parameter is only required when 'newdata' is a list of time series. Default value is 5.
#'
#' @return If 'newdata' is a list of time series, then a list of objects of class 'forecast' is returned. The 'plot' function in the R 'forecast' package can then be used to produce a plot of any time series in the returned list. Each list object contains the following properties.
#' \item{method}{The name of the forecasting method (SETAR-Forest) as a character string.}
#' \item{x}{The original time series.}
#' \item{mean}{Point forecasts as a time series.}
#' If 'newdata' is a dataframe/matrix, then a vector containing the prediction of each instance is returned.
#'
#' @examples
#' # Obtaining forecasts for a list of time series
#' forest1 <- setarforest(chaotic_logistic_series)
#' forecast(forest1, chaotic_logistic_series)
#'
#' # Obtaining forecasts for a set of test instances
#' forest2 <- setarforest(data = web_traffic_train[,-1],
#'                        label = web_traffic_train[,1],
#'                        stopping_criteria = "error_imp",
#'                        categorical_covariates = c("Project", "Access", "Agent"))
#' forecast(forest2, web_traffic_test)
#'
#' @export
forecast.setarforest <- function(forest, newdata, h = 5){
  if(class(forest) != "setarforest")
    stop("'forest' should be an object of class 'setarforest'")

  if(class(newdata) == "list"){
    if(length(newdata) < 1)
      stop("'newdata' should contain at least one time series.")

    if(forest$input_type != "list")
      stop("'newdata' is a list of time series. But the given 'setarforest' object is not fitted using a list of time series.")

    predictseries.setarforest(forest, newdata, h)
  }else if(class(newdata) == "data.frame" | "matrix" %in% class(newdata)){
    if(nrow(newdata) < 1)
      stop("'newdata' should contain at least one test instance.")

    if(forest$input_type != "df")
      stop("'newdata' is a dataframe/matrix. But the given 'setarforest' object is not fitted using a dataframe/matrix.")

    predict(forest, newdata)
  }else{
    stop("'newdata' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


# Function to fit a SETAR-Forest given a dataframe/matrix of inputs
fit.setarforest.df <- function(data, label, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", type = "df", verbose = 2, categorical_covariates = NULL){

  data <- as.data.frame(data)
  input_feature_names <- colnames(data)

  categorical_covariates_unique_vals <- NULL

  if(!is.null(categorical_covariates) & !all(categorical_covariates %in% colnames(data))){
    warning("One or more categorical features mentioned in 'categorical_covariates' are not available in 'data'.")
    categorical_covariates <- categorical_covariates[categorical_covariates %in% colnames(data)]

    if(length(categorical_covariates) == 0)
      categorical_covariates <- NULL
  }

  if(!is.null(categorical_covariates)){
    categorical_covariates_unique_vals <- list()
    categorical_indexes <- match(categorical_covariates, colnames(data))

    for(index in 1:length(categorical_indexes)){
      cat_col_index <- categorical_indexes[index]
      data <- cbind(data, do_one_hot_encoding(data[,cat_col_index], unique(data[,cat_col_index]), categorical_covariates[index]))
      categorical_covariates_unique_vals[[categorical_covariates[index]]] <- unique(data[,cat_col_index])
    }

    data <- data[,-categorical_indexes]
  }

  embed_data <- as.data.frame(cbind(label, data))
  colnames(embed_data)[1] <- "y"

  start_time <- Sys.time()

  if(verbose == 1 | verbose == 2)
    print("Started building SETAR-Forest")

  output.forest <- list()
  output.forest$trees <- list()

  num_indexes <- round(nrow(embed_data) * bagging_fraction)

  # Training multiple SETAR-Trees as required
  for(bag_f in 1:bagging_freq){
    if(verbose == 1 | verbose == 2)
      print(paste0("Started processing tree ", bag_f))

    set.seed(bag_f)
    tree_indexes <- sort(sample(1:nrow(embed_data), num_indexes, replace = FALSE))

    current_tree_data <- embed_data[tree_indexes,]

    if(random_tree_significance){
      set.seed(bag_f)
      significance <- sample(seq(0.01, 0.1, length.out = 10), 1)

      if(verbose == 2)
        print(paste0("Chosen significance for tree ", bag_f, ": ", significance))
    }

    if(random_tree_significance_divider){
      set.seed(bag_f)
      significance_divider <- sample(2:10, 1)

      if(verbose == 2)
        print(paste0("Chosen significance divider for tree ", bag_f, ": ", significance_divider))
    }

    if(random_tree_error_threshold){
      set.seed(bag_f)
      error_threshold <- sample(seq(0.001, 0.05, length.out = 50), 1)

      if(verbose == 2)
        print(paste0("Chosen error threshold for tree ", bag_f, ": ", error_threshold))
    }

    # Execute individual SETAR trees
    output.forest$trees[[bag_f]] <- setartree(current_tree_data[,-1], current_tree_data[,1], ncol(data), depth, significance, significance_divider, error_threshold, stopping_criteria, verbose)
  }

  end_time <- Sys.time()
  exec_time <- end_time - start_time

  output.forest$lag <- ncol(data)
  output.forest$feature_names <- input_feature_names
  output.forest$coefficients <- output.forest$trees[[1]]$coefficients
  output.forest$categorical_covariate_values <- categorical_covariates_unique_vals
  output.forest$input_type <- type
  output.forest$execution_time <- exec_time
  class(output.forest) <- "setarforest"

  if(verbose == 1 | verbose == 2)
    print("Finished building SETAR-Forest")

  output.forest
}


# Function to fit a SETAR-Forest given a list of time series
fit.setarforest.series <- function(time_series_list, lag = 10, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", verbose = 2){

  embedded_series <- NULL

  for (i in 1:length(time_series_list)) {
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    embedded <- embed(time_series, lag + 1)

    if (!is.null(embedded_series))
      embedded_series <- as.matrix(embedded_series)

    embedded_series <- rbind(embedded_series, embedded)
  }

  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")

  fit.setarforest.df(embedded_series[,-1], embedded_series[,1], bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, "list", verbose)
}


# Predict method for SETAR-Forest fits
predict.setarforest <- function(forest, newdata){
  newdata <- as.data.frame(newdata)

  if(!all(forest$feature_names %in% colnames(newdata))){
    stop("The features that are used to train the given 'setarforest' object and the features in the 'newdata' do not match.")
  }

  newdata <- newdata[, forest$feature_names]

  if(!is.null(forest$categorical_covariate_values)){
    categorical_covariates <- names(forest$categorical_covariate_values)
    cat_indexes <- match(categorical_covariates, colnames(newdata))

    for(l in 1:length(categorical_covariates)){
      trained_cat_values <- forest$categorical_covariate_values[[l]]
      test_cat_values <- unique(newdata[,categorical_covariates[l]])

      if(!all(test_cat_values %in% trained_cat_values)){
        stop(paste0("The feature ", categorical_covariates[l], " contains unseen values in 'newdata'. Cannot proceed."))
      }

      newdata <- cbind(newdata, do_one_hot_encoding(newdata[,categorical_covariates[l]], trained_cat_values, categorical_covariates[l]))
    }

    newdata <- newdata[,-cat_indexes]
  }

  if(!all(forest$coefficients %in% colnames(newdata)))
    stop("Coefficients of the given 'setarforest' object and the features in the 'newdata' do not match. Cannot proceed.")

  newdata <- newdata[,forest$coefficients]

  all_tree_models <- forest$trees

  predictions <- forecast(all_tree_models[[1]], newdata)

  for(t in 2:length(all_tree_models))
    predictions <- predictions + forecast(all_tree_models[[t]], newdata)

  predictions <- predictions/length(all_tree_models) # Final predictions are the average of predictions given by all trees

  predictions
}


# Function to obtain predictions for a given list of time series
predictseries.setarforest <- function(forest, time_series_list, h = 5){

  final_lags <- NULL
  forecasts <- NULL
  lag <- forest$lag
  coefficient_names <- forest$coefficients

  for (i in 1:length(time_series_list)) {
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))

    if (!is.null(final_lags))
      final_lags <- as.matrix(final_lags)

    final_lags <- rbind(final_lags, current_series_final_lags)
  }

  final_lags <- as.data.frame(final_lags)
  colnames(final_lags) <- coefficient_names

  for(ho in 1:h){
    horizon_predictions <- as.numeric(predict(forest, as.data.frame(final_lags)))
    forecasts <- cbind(forecasts, horizon_predictions)

    # Updating the test set for the next horizon
    if(ho < h){
      final_lags <- final_lags[-lag]

      # Updating lags for the next horizon
      final_lags <- cbind(horizon_predictions, final_lags)
      colnames(final_lags) <- coefficient_names
      final_lags <- as.data.frame(final_lags)
    }
  }

  results <- list()

  for(i in 1:length(time_series_list)){
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    current_result <- list()
    current_result$method = "SETAR-Forest"
    current_result$mean = ts(as.numeric(forecasts[i,]), start = length(time_series)+1)
    current_result$x = ts(time_series)
    class(current_result) <- "forecast"
    results[[i]] <- current_result
  }

  results
}



#' Fitting SETAR-Forest models
#'
#' Fits a SETAR-Forest model either using a list of time series or an embedded input matrix and labels.
#'
#' @param data A list of time series (each list element is a separate time series) or a dataframe/matrix containing model inputs (the columns can contain past time series lags and/or external numerical/categorical covariates).
#' @param label A vector of true outputs. This parameter is only required when \code{data} is a dataframe/matrix containing the model inputs.
#' @param lag The number of past time series lags that should be used when fitting each SETAR-Tree in the forest. This parameter is only required when \code{data} is a list of time series. Default value is 10.
#' @param bagging_fraction The percentage of instances that should be used to train each SETAR-Tree in the forest.  Default value is 0.8.
#' @param bagging_freq The number of SETAR-Trees in the forest.  Default value is 10.
#' @param random_tree_significance Whether a random significance should be considered for splitting per each tree. Each node split within the tree considers the same significance level. When this parameter is set to TRUE, the "significance" parameter will be ignored. Default value is TRUE.
#' @param random_tree_significance_divider Whether a random significance divider should be considered for splitting per each tree. When this parameter is set to TRUE, the "significance_divider" parameter will be ignored. Default value is TRUE.
#' @param random_tree_error_threshold Whether a random error threshold should be considered for splitting per each tree. Each node split within the tree considers the same error threshold. When this parameter is set to TRUE, the "error_threshold" parameter will be ignored. Default value is TRUE.
#' @param depth  Maximum depth of each SETAR-Tree in the forest. Default value is 1000. Thus, unless specify a lower value, the depth of a SETAR-Tree is actually controlled by the stopping criterion.
#' @param significance In each SETAR-Tree in the forest, the initial significance used by the linearity test (alpha_0). Default value is 0.05.
#' @param significance_divider In each SETAR-Tree in the forest, the corresponding significance in a tree level is divided by this value. Default value is 2.
#' @param error_threshold In each SETAR-Tree in the forest, the minimum error reduction percentage between parent and child nodes to make a split. Default value is 0.03.
#' @param stopping_criteria The required stopping criteria for each SETAR-Tree in the forest: linearity test (lin_test), error reduction percentage (error_imp) or linearity test and error reduction percentage (both). Default value is \code{"both"}.
#' @param mean_normalisation Whether each series should be normalised by deducting its mean value before building the forest. This parameter is only required when \code{data} is a list of time series. Default value is FALSE.
#' @param window_normalisation Whether the window-wise normalisation should be applied before building the forest. This parameter is only required when \code{data} is a list of time series. When this is TRUE, each row of the training embedded matrix is normalised by deducting its mean value before building the forest. Default value is FALSE.
#' @param verbose Controls the level of the verbosity of SETAR-Forest: 0 (errors/warnings), 1 (limited amount of information including the depth of the currently processing tree), 2 (full training information including the depth of the currently processing tree and stopping criterion related details in each tree). Default value is 2.
#' @param categorical_covariates Names of the categorical covariates in the input data. This parameter is only required when \code{data} is a dataframe/matrix and it contains categorical variables.
#'
#' @return An object of class \code{\link{setarforest}} which contains the following properties.
#' \item{trees}{A list of objects of class \code{\link{setartree}} which represents the trained SETAR-Tree models in the forest.}
#' \item{lag}{The number of features used to train each SEATR-Tree in the forest.}
#' \item{feature_names}{Names of the input features.}
#' \item{coefficients}{Names of the coefficients of leaf node regresion models in each SETAR-Tree in the forest.}
#' \item{categorical_covariate_values}{Information about the categorical covarites used during training (only if applicable).}
#' \item{mean_normalisation}{Whether mean normalisation was applied for the training data.}
#' \item{window_normalisation}{Whether window normalisation was applied for the training data.}
#' \item{input_type}{Type of input data used to train the SETAR-Forest. This is \code{list} if \code{data} is a list of time series, and \code{df} if \code{data} is a dataframe/matrix containing model inputs.}
#' \item{execution_time}{Execution time of SETAR-Forest.}
#'
#' @importFrom methods is
#' @importFrom utils lsf.str
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster 
#'
#' @examples
#' \donttest{
#' # Training SETAR-Forest with a list of time series
#' setarforest(chaotic_logistic_series, bagging_freq = 2)
#'
#' # Training SETAR-Forest with a dataframe containing model inputs where the model inputs may contain
#' # past time series lags and numerical/categorical covariates
#' setarforest(data = web_traffic_train[,-1],
#'             label = web_traffic_train[,1],
#'             bagging_freq = 2,
#'             categorical_covariates = "Project")
#' }
#'
#' @export
setarforest <- function(data, label = NULL, lag = 10, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, verbose = 2, categorical_covariates = NULL){

  if(random_tree_significance & (verbose == 1 | verbose == 2))
    message("'random_tree_significance' = TRUE ... Ignored 'significance' parameter.")
  if(random_tree_error_threshold & (verbose == 1 | verbose == 2))
    message("'random_tree_error_threshold' = TRUE ... Ignored 'error_threshold' parameter.")
  if(random_tree_significance_divider & (verbose == 1 | verbose == 2))
    message("'random_tree_significance_divider' = TRUE ... Ignored 'significance_divider' parameter.")

  if(is(data, "list")){
    if(length(data) < 1)
      stop("'data' should contain at least one time series.")
    if(!is.null(label)){
      warning("'data' is a list of time series. 'label' is ignored.")
    }
    if(!is.null(categorical_covariates)){
      warning("'data' is a list of time series. 'categorical_covariates' is ignored.")
    }
    fit.setarforest.series(data, lag, bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, verbose)
  }else if(is(data, "data.frame") |  is(data, "matrix")){
    if(is.null(label))
      stop("'label' is missing. Please provide the true outputs corresponding with each instance in 'data'.")
    if(mean_normalisation)
      warning("'data' is a dataframe/matrix. 'mean_normalisation' is ignored.")
    if(window_normalisation)
      warning("'data' is a dataframe/matrix. 'window_normalisation' is ignored.")
    fit.setarforest.df(data, label, bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, FALSE, FALSE, "df", verbose, categorical_covariates)
  }else{
    stop("'data' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


#' Forecast method for SETAR-Forest fits
#'
#' Obtains forecasts for a given set of time series or a dataframe/matrix of new instances from a fitted SETAR-Forest model.
#'
#' @param object An object of class \code{\link{setarforest}} which is a trained SETAR-Forest model.
#' @param newdata A list of time series which need forecasts or a dataframe/matrix of new instances which need predictions.
#' @param h The required number of forecasts (forecast horizon). This parameter is only required when \code{newdata} is a list of time series. Default value is 5.
#' @param level Confidence level for prediction intervals. Default value is c(80, 95).
#' @param ... Other arguments.
#'
#' @return If \code{newdata} is a list of time series, then an object of class \code{mforecast} is returned.
#' The \code{plot} or \code{autoplot} functions in the R \code{forecast} package can then be used to produce a plot of any time series in the returned object which contains the following properties.
#' \item{method}{A vector containing the name of the forecasting method ("SETAR-Forest").}
#' \item{forecast}{A list of objects of class \code{forecast}.
#' Each list object is corresponding with a time series and its forecasts.
#' Each list object contains 7 properties:
#' method (the name of the forecasting method, SETAR-Forest, as a character string),
#' x (the original time series),
#' mean (point forecasts as a time series),
#' series (the name of the series as a character string),
#' upper (upper bound of confidence intervals),
#' lower (lower bound of confidence intervals) and
#' level (confidence level of prediction intervals).}
#' If \code{newdata} is a dataframe/matrix, then a list containing the prediction and prediction intervals (upper and lower bounds) of each instance is returned.
#'
#' @importFrom methods is
#' @importFrom stats qnorm
#'
#' @examples
#' \donttest{
#' # Obtaining forecasts for a list of time series
#' forest1 <- setarforest(chaotic_logistic_series, bagging_freq = 2)
#' forecast(forest1, chaotic_logistic_series)
#'
#' # Obtaining forecasts for a set of test instances
#' forest2 <- setarforest(data = web_traffic_train[,-1],
#'                        label = web_traffic_train[,1],
#'                        bagging_freq = 2,
#'                        categorical_covariates = "Project")
#' forecast(forest2, web_traffic_test)
#' }
#'
#' @method forecast setarforest
#' @export
forecast.setarforest <- function(object, newdata, h = 5, level = c(80, 95), ...){
  if(!is(object, "setarforest"))
    stop("'object' should be an object of class 'setarforest'")

  if(min(level) > 0 && max(level) < 1) 
    level <- 100 * level
  else if(min(level) < 0 || max(level) > 99.99) 
    stop("Confidence limit out of range")
  
  level <- sort(level)
  
  if(is(newdata, "list")){
    if(length(newdata) < 1)
      stop("'newdata' should contain at least one time series.")

    if(object$input_type != "list")
      stop("'newdata' is a list of time series. But the given 'setarforest' object is not fitted using a list of time series.")

    predictseries.setarforest(object, newdata, h, level)
  }else if(is(newdata, "data.frame") | is(newdata, "matrix")){
    if(nrow(newdata) < 1)
      stop("'newdata' should contain at least one test instance.")

    if(object$input_type != "df")
      stop("'newdata' is a dataframe/matrix. But the given 'setarforest' object is not fitted using a dataframe/matrix.")

    predict(object, newdata, level)
  }else{
    stop("'newdata' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


# Function to fit a SETAR-Forest given a dataframe/matrix of inputs
fit.setarforest.df <- function(data, label, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, type = "df", verbose = 2, categorical_covariates = NULL){

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
    message("Started building SETAR-Forest")

  output.forest <- list()
  output.forest$trees <- list()

  num_indexes <- round(nrow(embed_data) * bagging_fraction)
  
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if(nzchar(chk) && chk == "TRUE")
    num_cores <- 1
  else
    num_cores <- parallel::detectCores()
  
  # Training multiple SETAR-Trees as required
  if(num_cores > 1){
    cluster <- parallel::makeCluster(num_cores)
    parallel::clusterExport(cluster, as.character(unclass(lsf.str(envir = asNamespace("setartree"), all = TRUE))))
    
    output.forest$trees <- parallel::parLapply(cluster, sapply(1:bagging_freq, list), function(bag_f){ 
      
      if(verbose == 1 | verbose == 2)
        message(paste0("Started processing tree ", bag_f))

      set.seed(bag_f)
      tree_indexes <- sort(sample(1:nrow(embed_data), num_indexes, replace = FALSE))

      current_tree_data <- embed_data[tree_indexes,]

      if(random_tree_significance){
        set.seed(bag_f)
        significance <- sample(seq(0.01, 0.1, length.out = 10), 1)

        if(verbose == 2)
          message(paste0("Chosen significance for tree ", bag_f, ": ", significance))
      }

      if(random_tree_significance_divider){
        set.seed(bag_f)
        significance_divider <- sample(2:10, 1)

        if(verbose == 2)
          message(paste0("Chosen significance divider for tree ", bag_f, ": ", significance_divider))
      }

      if(random_tree_error_threshold){
        set.seed(bag_f)
        error_threshold <- sample(seq(0.001, 0.05, length.out = 50), 1)

        if(verbose == 2)
          message(paste0("Chosen error threshold for tree ", bag_f, ": ", error_threshold))
      }

      # Execute individual SETAR trees
      setartree(current_tree_data[,-1], current_tree_data[,1], ncol(data), depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, verbose)
    })
    
    parallel::stopCluster(cluster)
  }else{
    for(bag_f in 1:bagging_freq){
      if(verbose == 1 | verbose == 2)
        message(paste0("Started processing tree ", bag_f))
      
      set.seed(bag_f)
      tree_indexes <- sort(sample(1:nrow(embed_data), num_indexes, replace = FALSE))
      
      current_tree_data <- embed_data[tree_indexes,]
      
      if(random_tree_significance){
        set.seed(bag_f)
        significance <- sample(seq(0.01, 0.1, length.out = 10), 1)
        
        if(verbose == 2)
          message(paste0("Chosen significance for tree ", bag_f, ": ", significance))
      }
      
      if(random_tree_significance_divider){
        set.seed(bag_f)
        significance_divider <- sample(2:10, 1)
        
        if(verbose == 2)
          message(paste0("Chosen significance divider for tree ", bag_f, ": ", significance_divider))
      }
      
      if(random_tree_error_threshold){
        set.seed(bag_f)
        error_threshold <- sample(seq(0.001, 0.05, length.out = 50), 1)
        
        if(verbose == 2)
          message(paste0("Chosen error threshold for tree ", bag_f, ": ", error_threshold))
      }
      
      # Execute individual SETAR trees
      output.forest$trees[[bag_f]] <- setartree(current_tree_data[,-1], current_tree_data[,1], ncol(data), depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, verbose)
    }
  }

  end_time <- Sys.time()
  exec_time <- end_time - start_time

  output.forest$lag <- ncol(data)
  output.forest$feature_names <- input_feature_names
  output.forest$coefficients <- output.forest$trees[[1]]$coefficients
  output.forest$categorical_covariate_values <- categorical_covariates_unique_vals
  output.forest$mean_normalisation <- mean_normalisation
  output.forest$window_normalisation <- window_normalisation
  output.forest$input_type <- type
  output.forest$execution_time <- exec_time
  class(output.forest) <- "setarforest"

  if(verbose == 1 | verbose == 2)
    message("Finished building SETAR-Forest")

  output.forest
}


# Function to fit a SETAR-Forest given a list of time series
fit.setarforest.series <- function(time_series_list, lag = 10, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_tree_significance_divider = TRUE, random_tree_error_threshold = TRUE, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, verbose = 2){

  embedded_series <- NULL

  for (i in 1:length(time_series_list)) {
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    
    if(mean_normalisation)
      time_series <- time_series - mean(time_series, na.rm = TRUE)
    
    embedded <- embed(time_series, lag + 1)

    if (!is.null(embedded_series))
      embedded_series <- as.matrix(embedded_series)

    embedded_series <- rbind(embedded_series, embedded)
  }
  
  if(window_normalisation)
    embedded_series <- embedded_series - rowMeans(embedded_series[,2:ncol(embedded_series)]) 

  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")

  fit.setarforest.df(embedded_series[,-1], embedded_series[,1], bagging_fraction, bagging_freq, random_tree_significance, random_tree_significance_divider, random_tree_error_threshold, depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, "list", verbose)
}


# Predict method for SETAR-Forest fits
predict.setarforest <- function(forest, newdata, level = c(80, 95)){
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

  tree_output <- forecast.setartree(all_tree_models[[1]], newdata, level = level)
  predictions <- tree_output[["predictions"]]
  pooled_var <- (tree_output[["size"]] - 1)*(tree_output[["stds"]]^2)
  full_size <- tree_output[["size"]] - 1

  for(t in 2:length(all_tree_models)){
    tree_output <- forecast.setartree(all_tree_models[[t]], newdata, level = level)
    predictions <- predictions + tree_output[["predictions"]]
    pooled_var <- pooled_var + ((tree_output[["size"]] - 1)*(tree_output[["stds"]]^2))
    full_size <- full_size + (tree_output[["size"]] - 1)
  }

  predictions <- predictions/length(all_tree_models) # Final predictions are the average of predictions given by all trees
  pooled_var <- pooled_var/full_size
  stds <- sqrt(pooled_var)
  
  lower_intervals <- NULL
  upper_intervals <- NULL
  
  for(le in level){
    multiplier <- abs(qnorm((100 - le)/200))
    lower_intervals <- cbind(lower_intervals,  (predictions - multiplier*stds))
    upper_intervals <- cbind(upper_intervals,  (predictions + multiplier*stds))
  }
  
  colnames(lower_intervals) <- colnames(upper_intervals) <- as.character(level)
  
  list("predictions" = as.numeric(predictions), 
       "lower_intervals" = as.data.frame(lower_intervals), 
       "upper_intervals" = as.data.frame(upper_intervals)
  )
}


# Function to obtain predictions for a given list of time series
predictseries.setarforest <- function(forest, time_series_list, h = 5, level = c(80, 95)){

  final_lags <- NULL
  forecasts <- NULL
  
  series_means <- NULL
  
  for(le in level){
    assign(paste0("all_lower_", le, "_intervals"), NULL)
    assign(paste0("all_upper_", le, "_intervals"), NULL)
  }
  
  lag <- forest$lag
  coefficient_names <- forest$coefficients

  for (i in 1:length(time_series_list)) {
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    
    if(forest$mean_normalisation){
      mean_val <- mean(time_series, na.rm = TRUE)
      series_means <- c(series_means, mean_val)
      time_series <- time_series - mean_val
    }
    
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))

    if (!is.null(final_lags))
      final_lags <- as.matrix(final_lags)

    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  if(forest$window_normalisation){
    final_lags_row_means <- rowMeans(final_lags) 
    final_lags <- final_lags - final_lags_row_means
  }

  final_lags <- as.data.frame(final_lags)
  colnames(final_lags) <- coefficient_names

  for(ho in 1:h){
    horizon_output <- predict(forest, as.data.frame(final_lags), level)
    horizon_predictions <- horizon_output[["predictions"]]
    
    if(forest$window_normalisation)
      horizon_predictions <- horizon_predictions + final_lags_row_means
    
    forecasts <- cbind(forecasts, horizon_predictions)
    
    if(forest$window_normalisation){
      for(le in level){
        assign(paste0("all_lower_", le, "_intervals"), cbind(eval(parse(text=paste0("all_lower_", le, "_intervals"))), (eval(parse(text=paste0("horizon_output[['lower_intervals']][[as.character(", le, ")]]"))) + final_lags_row_means)))
        assign(paste0("all_upper_", le, "_intervals"), cbind(eval(parse(text=paste0("all_upper_", le, "_intervals"))), (eval(parse(text=paste0("horizon_output[['upper_intervals']][[as.character(", le, ")]]"))) + final_lags_row_means)))
      }
    }else{
      for(le in level){
        assign(paste0("all_lower_", le, "_intervals"), cbind(eval(parse(text=paste0("all_lower_", le, "_intervals"))), eval(parse(text=paste0("horizon_output[['lower_intervals']][[as.character(", le, ")]]")))))
        assign(paste0("all_upper_", le, "_intervals"), cbind(eval(parse(text=paste0("all_upper_", le, "_intervals"))), eval(parse(text=paste0("horizon_output[['upper_intervals']][[as.character(", le, ")]]")))))
      }
    }

    # Updating the test set for the next horizon
    if(ho < h){
      final_lags <- final_lags[-lag]
      
      if(forest$window_normalisation)
        final_lags <- final_lags + final_lags_row_means

      # Updating lags for the next horizon
      final_lags <- cbind(horizon_predictions, final_lags)
      
      if(forest$window_normalisation){
        final_lags_row_means <- rowMeans(final_lags) 
        final_lags <- final_lags - final_lags_row_means
      }
      
      colnames(final_lags) <- coefficient_names
      final_lags <- as.data.frame(final_lags)
    }
  }

  if(forest$mean_normalisation){
    forecasts <- forecasts + series_means
    
    for(le in level){
      assign(paste0("all_lower_", le, "_intervals"), (eval(parse(text=paste0("all_lower_", le, "_intervals"))) + series_means))
      assign(paste0("all_upper_", le, "_intervals"), (eval(parse(text=paste0("all_upper_", le, "_intervals"))) + series_means))
    }
  }
  
  results <- list()
  results$method <- rep("SETAR-Forest", length(time_series_list))
  names(results$method) <- paste0("T", 1:length(time_series_list))
  results$forecast <- list()

  for(i in 1:length(time_series_list)){
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    current_result <- list()
    current_result$method = "SETAR-Forest"
    current_result$mean = ts(as.numeric(forecasts[i,]), start = length(time_series)+1)
    current_result$x = ts(time_series)
    current_result$series <- paste0("T", i)
    current_result$level <- level
    
    u <- NULL
    l <- NULL
    
    for(le in level){
      u <- cbind(u, eval(parse(text=paste0("as.numeric(all_upper_", le, "_intervals[i,])"))))
      l <- cbind(l, eval(parse(text=paste0("as.numeric(all_lower_", le, "_intervals[i,])"))))
    }
    
    current_result$upper = ts(as.matrix(u), start = length(time_series)+1)
    current_result$lower = ts(as.matrix(l), start = length(time_series)+1)
    colnames(current_result$upper) <- colnames(current_result$lower) <- paste0(level, "%")
    
    class(current_result) <- "forecast"
    results$forecast[[paste0("T", i)]] <- current_result
  }

  class(results) <- "mforecast"
  results
}



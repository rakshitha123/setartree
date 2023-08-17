#' Fitting SETAR-Tree models
#'
#' Fits a SETAR-Tree model either using a list of time series or an embedded input matrix and labels.
#'
#' @param data A list of time series (each list element is a separate time series) or a dataframe/matrix containing model inputs (the columns can contain past time series lags and/or external numerical/categorical covariates).
#' @param label A vector of true outputs. This parameter is only required when \code{data} is a dataframe/matrix containing the model inputs.
#' @param lag The number of past time series lags that should be used when fitting the SETAR-Tree. This parameter is only required when \code{data} is a list of time series. Default value is 10.
#' @param depth  Maximum tree depth. Default value is 1000. Thus, unless specify a lower value, the depth is actually controlled by the stopping criterion.
#' @param significance Initial significance used by the linearity test (alpha_0). Default value is 0.05.
#' @param significance_divider The corresponding significance in each tree level is divided by this value. Default value is 2.
#' @param error_threshold The minimum error reduction percentage between parent and child nodes to make a split. Default value is 0.03.
#' @param stopping_criteria The required stopping criteria: linearity test (lin_test), error reduction percentage (error_imp) or linearity test and error reduction percentage (both). Default value is \code{"both"}.
#' @param mean_normalisation Whether each series should be normalised by deducting its mean value before building the tree. This parameter is only required when \code{data} is a list of time series. Default value is FALSE.
#' @param window_normalisation Whether the window-wise normalisation should be applied before building the tree. This parameter is only required when \code{data} is a list of time series. When this is TRUE, each row of the training embedded matrix is normalised by deducting its mean value before building the tree. Default value is FALSE.
#' @param verbose Controls the level of the verbosity of SETAR-Tree: 0 (errors/warnings), 1 (limited amount of information including the current tree depth), 2 (full training information including the current tree depth and stopping criterion results in each tree node). Default value is 2.
#' @param categorical_covariates Names of the categorical covariates in the input data. This parameter is only required when \code{data} is a dataframe/matrix and it contains categorical variables.
#'
#' @return An object of class \code{\link{setartree}} which contains the following properties.
#' \item{leaf_models}{Trained global pooled regression models in each leaf node.}
#' \item{opt_lags}{Optimal features used to split each node.}
#' \item{opt_thresholds}{Optimal threshold values used to split each node.}
#' \item{lag}{The number of features used to train the SETAR-Tree.}
#' \item{feature_names}{Names of the input features.}
#' \item{coefficients}{Names of the coefficients of leaf node regresion models.}
#' \item{num_leaves}{Number of leaf nodes in the SETAR-Tree.}
#' \item{depth}{Depth of the SETAR-Tree which was determined based on the specified stopping criterion.}
#' \item{leaf_instance_dis}{Number of instances used to train the regression models at each leaf node.}
#' \item{stds}{The standard deviations of the residuals of each leaf node.}
#' \item{categorical_covariate_values}{Information about the categorical covarites used during training (only if applicable).}
#' \item{mean_normalisation}{Whether mean normalisation was applied for the training data.}
#' \item{window_normalisation}{Whether window normalisation was applied for the training data.}
#' \item{input_type}{Type of input data used to train the SETAR-Tree. This is \code{list} if \code{data} is a list of time series, and \code{df} if \code{data} is a dataframe/matrix containing model inputs.}
#' \item{execution_time}{Execution time of SETAR-Tree.}
#'
#' @importFrom stats as.formula embed glm pf predict predict.glm ts
#' @importFrom utils tail
#' @importFrom methods is
#'
#' @examples
#' \donttest{
#' # Training SETAR-Tree with a list of time series
#' setartree(chaotic_logistic_series)
#'
#' # Training SETAR-Tree with a dataframe containing model inputs where the model inputs may contain
#' # past time series lags and numerical/categorical covariates
#' setartree(data = web_traffic_train[,-1],
#'           label = web_traffic_train[,1],
#'           stopping_criteria = "both",
#'           categorical_covariates = "Project")
#' }
#'
#' @export
setartree <- function(data, label = NULL, lag = 10, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, verbose = 2, categorical_covariates = NULL){
  if(is(data, "list")){
    if(length(data) < 1)
      stop("'data' should contain at least one time series.")
    if(!is.null(label)){
      warning("'data' is a list of time series. 'label' is ignored.")
    }
    if(!is.null(categorical_covariates)){
      warning("'data' is a list of time series. 'categorical_covariates' is ignored.")
    }
    fit.setartree.series(data, lag, depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, verbose)
  }else if(is(data, "data.frame") |  is(data, "matrix")){
    if(is.null(label))
      stop("'label' is missing. Please provide the true outputs corresponding with each instance in 'data'.")
    fit.setartree.df(data, label, depth, significance, significance_divider, error_threshold, stopping_criteria, FALSE, FALSE, "df", verbose, categorical_covariates)
  }else{
    stop("'data' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


#' Forecast method for SETAR-Tree fits
#'
#' Obtains forecasts for a given set of time series or a dataframe/matrix of new instances from a fitted SETAR-Tree model.
#'
#' @param object An object of class \code{\link{setartree}} which is a trained SETAR-Tree model.
#' @param newdata A list of time series which needs forecasts or a dataframe/matrix of new instances which need predictions.
#' @param h The required number of forecasts (forecast horizon). This parameter is only required when \code{newdata} is a list of time series. Default value is 5.
#' @param level Confidence level for prediction intervals. Default value is c(80, 95).
#' @param ... Other arguments.
#'
#' @return If \code{newdata} is a list of time series, then an object of class \code{mforecast} is returned.
#' The \code{plot} or \code{autoplot} functions in the R \code{forecast} package can then be used to produce a plot of any time series in the returned object which contains the following properties.
#' \item{method}{A vector containing the name of the forecasting method ("SETAR-Tree").}
#' \item{forecast}{A list of objects of class \code{forecast}.
#' Each list object is corresponding with a time series and its forecasts.
#' Each list object contains 7 properties:
#' method (the name of the forecasting method, SETAR-Tree, as a character string),
#' x (the original time series),
#' mean (point forecasts as a time series),
#' series (the name of the series as a character string),
#' upper (upper bound of confidence intervals),
#' lower (lower bound of confidence intervals) and
#' level (confidence level of prediction intervals).}
#' If \code{newdata} is a dataframe/matrix, then a list containing the predictions, prediction intervals (upper and lower bounds), the size and standard deviations of the residuals of the models used to get each prediction is returned.
#'
#' @importFrom methods is
#'
#' @examples
#' \donttest{
#' # Obtaining forecasts for a list of time series
#' tree1 <- setartree(chaotic_logistic_series)
#' forecast(tree1, chaotic_logistic_series)
#'
#' # Obtaining forecasts for a set of test instances
#' tree2 <- setartree(data = web_traffic_train[,-1],
#'                    label = web_traffic_train[,1],
#'                    stopping_criteria = "both",
#'                    categorical_covariates = "Project")
#' forecast(tree2, web_traffic_test)
#' }
#'
#' @method forecast setartree
#' @export
forecast.setartree <- function(object, newdata, h = 5, level = c(80, 95), ...){
  if(!is(object, "setartree"))
    stop("'object' should be an object of class 'setartree'")

  if(min(level) > 0 && max(level) < 1) 
    level <- 100 * level
  else if(min(level) < 0 || max(level) > 99.99) 
    stop("Confidence limit out of range")
  
  level <- sort(level)
  
  if(is(newdata, "list")){
    if(length(newdata) < 1)
      stop("'newdata' should contain at least one time series.")

    if(object$input_type != "list")
      stop("'newdata' is a list of time series. But the given 'setartree' object is not fitted using a list of time series.")

    predictseries.setartree(object, newdata, h, level)
  }else if(is(newdata, "data.frame") |  is(newdata, "matrix")){
    if(nrow(newdata) < 1)
      stop("'newdata' should contain at least one test instance.")

    if(object$input_type != "df")
      stop("'newdata' is a dataframe/matrix. But the given 'setartree' object is not fitted using a dataframe/matrix.")

    predict(object, newdata, level)
  }else{
    stop("'newdata' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


# Function to fit a SETAR-Tree given a dataframe/matrix of inputs
fit.setartree.df <- function(data, label, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, type = "df", verbose = 2, categorical_covariates = NULL){

  # Set list of defaults
  start.con <- list(nTh = 15) # Number of thresholds considered when making each split to define the optimal lag/feature

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

  tree <- list() # Stores the nodes in tree in each level
  tree_models <- list() # Stores the models in tree in each level
  th_lags <- list() # Stores the optimal lags/features used during splitting
  thresholds <- list() # Stores the optimal thresholds used during splitting
  level_errors <- NULL
  
  node_data <- list(embed_data) # Root node contains the training instances coming from all series
  node_model <- fit_global_model(embed_data)
  node_model$RSS <- sum((embed_data$y - node_model[["predictions"]])^2)
  node_models <- list(node_model)
  
  split_info <- 1


  start_time <- Sys.time()

  if(verbose == 1 | verbose == 2)
    message("Started building SETAR-Tree")

  for(d in 1:depth){
    if(verbose == 1 | verbose == 2)
      message(paste0("Depth: ", d))

    level_th_lags <- NULL
    level_thresholds <- NULL
    level_nodes <- list()
    level_models <- list()
    level_significant_node_count <- 0
    level_split_info <- NULL

    for(n in 1:length(node_data)){
      if(verbose == 2)
        message(paste0("Node ", n))

      best_cost <- Inf
      th <- NULL
      th_lag <- NULL
      left_model <- NULL
      right_model <- NULL
      
      if((nrow(node_data[[n]]) >  (2 * ncol(data) + 2)) & split_info[n] == 1){

        for(lg in 1:ncol(data)){
          # Finding the optimal lag and threshold that should be used for splitting
          # Optimised grid search
          ss_output <- find.cut.point(as.matrix(node_data[[n]][,-1]), as.matrix(node_data[[n]][,1]), node_data[[n]][,lg+1], start.con$nTh)
          cost <- ss_output[["RSS.left"]] + ss_output[["RSS.right"]]
          recheck <- ss_output[["need_recheck"]]

          if(cost <= best_cost) { # Find th and th_lag which minimize the squared errors
            best_cost <- cost
            th <- ss_output[["cut.point"]]
            th_lag <- lg
            left_model <- ss_output[["left.model"]]
            right_model <- ss_output[["right.model"]]
          }
        }

        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th) # Get the child nodes
          
          child_models <- list(left_model, right_model)
          
          # Check whether making the split is worth enough
          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_models[[n]], child_models, node_data[[n]], significance, verbose)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_models[[n]], child_models, error_threshold, verbose)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_models[[n]], child_models, node_data[[n]], significance, verbose) & check_error_improvement(node_models[[n]], child_models, error_threshold, verbose) 
          
          if(is_significant){ # Split the node into child nodes only if making that split is worth enough
            level_th_lags <- c(level_th_lags, th_lag)
            level_thresholds <- c(level_thresholds, th)
            level_split_info <- c(level_split_info, rep(1, 2))
            level_significant_node_count <- level_significant_node_count + 1

            for(s in 1:length(splited_nodes)){
              len <- length(level_nodes)
              level_nodes[[len + 1]] <- splited_nodes[[s]]
              level_models[[len + 1]] <- child_models[[s]]
            }
          }else{
            level_th_lags <- c(level_th_lags, 0)
            level_thresholds <- c(level_thresholds, 0)
            level_split_info <- c(level_split_info, 0)

            len <- length(level_nodes)
            level_nodes[[len + 1]] <- node_data[[n]]
            level_models[[len + 1]] <- node_models[[n]]
          }
        }else{
          level_th_lags <- c(level_th_lags, 0)
          level_thresholds <- c(level_thresholds, 0)
          level_split_info <- c(level_split_info, 0)

          len <- length(level_nodes)
          level_nodes[[len + 1]] <- node_data[[n]]
          level_models[[len + 1]] <- node_models[[n]]
        }
      }else{
        level_th_lags <- c(level_th_lags, 0)
        level_thresholds <- c(level_thresholds, 0)
        level_split_info <- c(level_split_info, 0)

        len <- length(level_nodes)
        level_nodes[[len + 1]] <- node_data[[n]]
        level_models[[len + 1]] <- node_models[[n]]
      }
    }

    if(level_significant_node_count > 0){
      tree[[d]] <- level_nodes
      tree_models[[d]] <- level_models
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      node_models <- tree_models[[d]]
      split_info <- level_split_info
      significance <- significance/significance_divider # Defining the significance for the next level of the tree
    }else
      break # If all nodes in a particular tree level are not further split, then stop
  }


  # Model training
  # Check whether the tree has any nodes. If not, train a single pooled regression model
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    leaf_trained_models <- tree_models[[length(tree_models)]]
    num_of_leaf_instances <- length(leaf_nodes)
    leaf_stds_type_1 <- NULL
    leaf_instance_dis <- NULL

    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){ 
      leaf_residuals <- leaf_nodes[[ln]]$y - as.numeric(predict.my.lm(leaf_trained_models[[ln]], as.matrix(leaf_nodes[[ln]][,2:ncol(leaf_nodes[[ln]])])))
      leaf_stds_type_1 <- c(leaf_stds_type_1, sqrt(sum(leaf_residuals^2)/(nrow(leaf_nodes[[ln]]) - ncol(data))))
      leaf_instance_dis <- c(leaf_instance_dis, nrow(leaf_nodes[[ln]]))
    }

    coefficients <- colnames(data)
  }else{
    final_result <- fit_global_model(embed_data)
    leaf_trained_models <- final_result[["model"]]
    num_of_leaf_instances <- 1
    residuals <- embed_data$y - final_result[["predictions"]]
    leaf_stds_type_1 <- sqrt(sum(residuals^2)/(nrow(embed_data) - ncol(data)))
    leaf_instance_dis <- nrow(embed_data)
    coefficients <- names(leaf_trained_models$coefficients)
  }

  end_time <- Sys.time()
  exec_time <- end_time - start_time

  output.tree <- list()
  output.tree$leaf_models <- leaf_trained_models
  output.tree$opt_lags <- th_lags
  output.tree$opt_thresholds <- thresholds
  output.tree$lag <- ncol(data)
  output.tree$feature_names <- input_feature_names
  output.tree$coefficients <- coefficients
  output.tree$num_leaves <- num_of_leaf_instances
  output.tree$depth <- length(tree)
  output.tree$leaf_instance_dis <- leaf_instance_dis
  output.tree$stds <- leaf_stds_type_1
  output.tree$categorical_covariate_values <- categorical_covariates_unique_vals
  output.tree$mean_normalisation <- mean_normalisation
  output.tree$window_normalisation <- window_normalisation
  output.tree$input_type <- type
  output.tree$execution_time <- exec_time
  class(output.tree) <- "setartree"

  if(verbose == 1 | verbose == 2)
    message("Finished building SETAR-Tree")

  output.tree
}


# Function to fit a SETAR-Tree given a list of time series
fit.setartree.series <- function(time_series_list, lag = 10, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", mean_normalisation = FALSE, window_normalisation = FALSE, verbose = 2){

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

  fit.setartree.df(embedded_series[,-1], embedded_series[,1], depth, significance, significance_divider, error_threshold, stopping_criteria, mean_normalisation, window_normalisation, "list", verbose)
}


# Predict method for SETAR-Tree fits
predict.setartree <- function(tree, newdata, level = c(80, 95)){
  newdata <- as.data.frame(newdata)

  if(!all(tree$feature_names %in% colnames(newdata))){
    stop("The features that are used to train the given 'setartree' object and the features in the 'newdata' do not match.")
  }

  newdata <- newdata[, tree$feature_names]

  if(!is.null(tree$categorical_covariate_values)){
    categorical_covariates <- names(tree$categorical_covariate_values)
    cat_indexes <- match(categorical_covariates, colnames(newdata))

    for(l in 1:length(categorical_covariates)){
      trained_cat_values <- tree$categorical_covariate_values[[l]]
      test_cat_values <- unique(newdata[,categorical_covariates[l]])

      if(!all(test_cat_values %in% trained_cat_values)){
        stop(paste0("The feature ", categorical_covariates[l], " contains unseen values in 'newdata'. Cannot proceed."))
      }

      newdata <- cbind(newdata, do_one_hot_encoding(newdata[,categorical_covariates[l]], trained_cat_values, categorical_covariates[l]))
    }

    newdata <- newdata[,-cat_indexes]
  }

  if(!all(tree$coefficients %in% colnames(newdata)))
    stop("Coefficients of the given 'setartree' object and the features in the 'newdata' do not match. Cannot proceed.")

  newdata <- newdata[,tree$coefficients]
  all_leaf_models <- tree$leaf_models
  all_stds <- tree$stds
  all_num_instances <- tree$leaf_instance_dis

  predictions <- NULL
  stds <- NULL
  num_instances <- NULL
  
  for(le in level){
    assign(paste0("lower_", le, "_intervals"), NULL)
    assign(paste0("upper_", le, "_intervals"), NULL)
  }

  if(tree$num_leaves > 1){
    for(r in 1:nrow(newdata)){
      leaf_index <- get_leaf_index(newdata[r,], tree$opt_lags, tree$opt_thresholds) # Identify the leaf node corresponding with the new data
      pred <- predict.my.lm(all_leaf_models[[leaf_index]], as.matrix(newdata[r,]))
      
      predictions <- c(predictions, pred)
      stds <- c(stds, all_stds[leaf_index])
      num_instances <- c(num_instances, all_num_instances[leaf_index])
        
      for(le in level){
        multiplier <- abs(qnorm((100 - le)/200))
        assign(paste0("lower_", le, "_intervals"), c(eval(parse(text=paste0("lower_", le, "_intervals"))), pred - multiplier*all_stds[leaf_index]))
        assign(paste0("upper_", le, "_intervals"), c(eval(parse(text=paste0("upper_", le, "_intervals"))), pred + multiplier*all_stds[leaf_index]))
      }
    }
  }else{
    predictions <- predict.glm(object = all_leaf_models, newdata = newdata)
    stds <- rep(all_stds, length(predictions))
    num_instances <- rep(all_num_instances, length(predictions))
    
    for(le in level){
      multiplier <- abs(qnorm((100 - le)/200))
      assign(paste0("lower_", le, "_intervals"), predictions - multiplier*all_stds)
      assign(paste0("upper_", le, "_intervals"), predictions + multiplier*all_stds)
    }
  }

  lower_intervals <- NULL
  upper_intervals <- NULL
  
  for(le in level){
    lower_intervals <- cbind(lower_intervals, eval(parse(text = paste0("lower_", le, "_intervals"))))
    upper_intervals <- cbind(upper_intervals, eval(parse(text = paste0("upper_", le, "_intervals"))))
  }
  
  colnames(lower_intervals) <- colnames(upper_intervals) <- as.character(level)
  
  list("predictions" = as.numeric(predictions), 
       "lower_intervals" = as.data.frame(lower_intervals), 
       "upper_intervals" = as.data.frame(upper_intervals),
       "stds" = as.numeric(stds),
       "size" = as.numeric(num_instances)
       )
}


# Function to obtain predictions for a given list of time series
predictseries.setartree <- function(tree, time_series_list, h = 5, level = c(80, 95)){

  final_lags <- NULL
  forecasts <- NULL
  
  series_means <- NULL
  
  for(le in level){
    assign(paste0("all_lower_", le, "_intervals"), NULL)
    assign(paste0("all_upper_", le, "_intervals"), NULL)
  }
 
  lag <- tree$lag
  coefficient_names <- tree$coefficients

  for(i in 1:length(time_series_list)){
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    
    if(tree$mean_normalisation){
      mean_val <- mean(time_series, na.rm = TRUE)
      series_means <- c(series_means, mean_val)
      time_series <- time_series - mean_val
    }
    
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))

    if(!is.null(final_lags))
      final_lags <- as.matrix(final_lags)

    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  if(tree$window_normalisation){
    final_lags_row_means <- rowMeans(final_lags) 
    final_lags <- final_lags - final_lags_row_means
  }

  final_lags <- as.data.frame(final_lags)
  colnames(final_lags) <- coefficient_names

  for(ho in 1:h){
    horizon_output <- predict(tree, as.data.frame(final_lags), level)
    horizon_predictions <- horizon_output[["predictions"]] 
    
    if(tree$window_normalisation)
      horizon_predictions <- horizon_predictions + final_lags_row_means
    
    forecasts <- cbind(forecasts, horizon_predictions)
    
    if(tree$window_normalisation){
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
      
      if(tree$window_normalisation)
        final_lags <- final_lags + final_lags_row_means

      # Updating lags for the next horizon
      final_lags <- cbind(horizon_predictions, final_lags)
      
      if(tree$window_normalisation){
        final_lags_row_means <- rowMeans(final_lags) 
        final_lags <- final_lags - final_lags_row_means
      }
      
      colnames(final_lags) <- coefficient_names
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  if(tree$mean_normalisation){
    forecasts <- forecasts + series_means
    
    for(le in level){
      assign(paste0("all_lower_", le, "_intervals"), (eval(parse(text=paste0("all_lower_", le, "_intervals"))) + series_means))
      assign(paste0("all_upper_", le, "_intervals"), (eval(parse(text=paste0("all_upper_", le, "_intervals"))) + series_means))
    }
  }

  results <- list()
  results$method <- rep("SETAR-Tree", length(time_series_list))
  names(results$method) <- paste0("T", 1:length(time_series_list))
  results$forecast <- list()

  for(i in 1:length(time_series_list)){
    time_series <- as.numeric(unlist(time_series_list[i], use.names = FALSE))
    current_result <- list()
    current_result$method <-  "SETAR-Tree"
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



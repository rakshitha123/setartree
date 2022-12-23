#' Fitting SETAR-Tree models
#'
#' Fits a SETAR-Tree model either using a list of time series or an embedded input matrix and labels.
#'
#' @param data A list of time series (each list element is a separate time series) or a dataframe/matrix containing model inputs (the columns can contain past time series lags and/or external numerical/categorical covariates).
#' @param label A vector of true outputs. This parameter is only required when 'data' is a dataframe/matrix containing the model inputs.
#' @param lag The number of past time series lags that should be used when fitting the SETAR-Tree. This parameter is only required when 'data' is a list of time series. Default value is 10.
#' @param depth  Maximum tree depth. Default value is 1000. Thus, unless specify a lower value, the depth is actually controlled by the stopping criterion.
#' @param significance Initial significance used by the linearity test (alpha_0). Default value is 0.05.
#' @param significance_divider The corresponding significance in each tree level is divided by this value. Default value is 2.
#' @param error_threshold The minimum error reduction percentage between parent and child nodes to make a split. Default value is 0.03.
#' @param stopping_criteria The required stopping criteria: linearity test (lin_test), error reduction percentage (error_imp) or linearity test and error reduction percentage (both). Default value is 'both'.
#' @param verbose Controls the level of the verbosity of SETAR-Tree: 0 (errors/warnings), 1 (limited amount of information including the current tree depth), 2 (full training information including the current tree depth and stopping criterion results in each tree node). Default value is 2.
#' @param categorical_covariates Names of the categorical covariates in the input data. This parameter is only required when 'data' is a dataframe/matrix and it contains categorical variables.
#'
#' @return An object of class 'setartree' which contains the following properties.
#' \item{leaf_models}{Trained global pooled regression models in each leaf node.}
#' \item{opt_lags}{Optimal features used to split each node.}
#' \item{opt_thresholds}{Optimal threshold values used to split each node.}
#' \item{lag}{The number of features used to train the SETAR-Tree.}
#' \item{feature_names}{Names of the input features.}
#' \item{coefficients}{Names of the coefficients of leaf node regresion models.}
#' \item{num_leaves}{Number of leaf nodes in the SETAR-Tree.}
#' \item{depth}{Depth of the SETAR-Tree which was determined based on the specified stopping criterion.}
#' \item{leaf_instance_dis}{Number of instances used to train the regression models at each leaf node.}
#' \item{categorical_covariate_values}{Information about the categorical covarites used during training (only if applicable).}
#' \item{input_type}{Type of input data used to train the SETAR-Tree. This is 'list' if 'data' is a list of time series, and 'df' if 'data' is a dataframe/matrix containing model inputs.}
#' \item{execution_time}{Execution time of SETAR-Tree.}
#'
#' @importFrom stats as.formula embed glm pf predict predict.glm ts
#' @importFrom utils tail
#' @importFrom methods is
#'
#' @examples
#' # Training SETAR-Tree with a list of time series
#' setartree(chaotic_logistic_series)
#'
#' # Training SETAR-Tree with a dataframe containing model inputs where the model inputs may contain
#' # past time series lags and numerical/categorical covariates
#' setartree(data = web_traffic_train[,-1],
#'           label = web_traffic_train[,1],
#'           stopping_criteria = "error_imp",
#'           categorical_covariates = c("Project", "Access", "Agent"))
#'
#' @export
setartree <- function(data, label = NULL, lag = 10, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", verbose = 2, categorical_covariates = NULL){
  if(is(data, "list")){
    if(length(data) < 1)
      stop("'data' should contain at least one time series.")
    if(!is.null(label)){
      print("'data' is a list of time series. 'label' is ignored.")
    }
    if(!is.null(categorical_covariates)){
      print("'data' is a list of time series. 'categorical_covariates' is ignored.")
    }
    fit.setartree.series(data, lag, depth, significance, significance_divider, error_threshold, stopping_criteria, verbose)
  }else if(is(data, "data.frame") |  is(data, "matrix")){
    if(is.null(label))
      stop("'label' is missing. Please provide the true outputs corresponding with each instance in 'data'.")
    fit.setartree.df(data, label, depth, significance, significance_divider, error_threshold, stopping_criteria, "df", verbose, categorical_covariates)
  }else{
    stop("'data' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


#' Forecast method for SETAR-Tree fits
#'
#' Obtains forecasts for a given set of time series or a dataframe/matrix of new instances from a fitted SETAR-Tree model.
#'
#' @param object An object of class 'setartree' which is a trained SETAR-Tree model.
#' @param newdata A list of time series which needs forecasts or a dataframe/matrix of new instances which need predictions.
#' @param h The required number of forecasts (forecast horizon). This parameter is only required when 'newdata' is a list of time series. Default value is 5.
#'
#' @return If 'newdata' is a list of time series, then a list of objects of class 'forecast' is returned. The 'plot' function in the R 'forecast' package can then be used to produce a plot of any time series in the returned list. Each list object contains the following properties.
#' \item{method}{The name of the forecasting method (SETAR-Tree) as a character string.}
#' \item{x}{The original time series.}
#' \item{mean}{Point forecasts as a time series.}
#' If 'newdata' is a dataframe/matrix, then a vector containing the prediction of each instance is returned.
#'
#' @importFrom methods is
#'
#' @examples
#' # Obtaining forecasts for a list of time series
#' tree1 <- setartree(chaotic_logistic_series)
#' forecast(tree1, chaotic_logistic_series)
#'
#' # Obtaining forecasts for a set of test instances
#' tree2 <- setartree(data = web_traffic_train[,-1],
#'                    label = web_traffic_train[,1],
#'                    stopping_criteria = "error_imp",
#'                    categorical_covariates = c("Project", "Access", "Agent"))
#' forecast(tree2, web_traffic_test)
#'
#' @export
forecast.setartree <- function(object, newdata, h = 5){
  if(!is(object, "setartree"))
    stop("'object' should be an object of class 'setartree'")

  if(is(newdata, "list")){
    if(length(newdata) < 1)
      stop("'newdata' should contain at least one time series.")

    if(object$input_type != "list")
      stop("'newdata' is a list of time series. But the given 'setartree' object is not fitted using a list of time series.")

    predictseries.setartree(object, newdata, h)
  }else if(is(newdata, "data.frame") |  is(newdata, "matrix")){
    if(nrow(newdata) < 1)
      stop("'newdata' should contain at least one test instance.")

    if(object$input_type != "df")
      stop("'newdata' is a dataframe/matrix. But the given 'setartree' object is not fitted using a dataframe/matrix.")

    predict(object, newdata)
  }else{
    stop("'newdata' should be either a list of time series or a dataframe/matrix containing model inputs.")
  }
}


# Function to fit a SETAR-Tree given a dataframe/matrix of inputs
fit.setartree.df <- function(data, label, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", type = "df", verbose = 2, categorical_covariates = NULL){

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
  th_lags <- list() # Stores the optimal lags/features used during splitting
  thresholds <- list() # Stores the optimal thresholds used during splitting
  level_errors <- NULL
  node_data <- list(embed_data) # Root node contains the training instances coming from all series
  split_info <- 1


  start_time <- Sys.time()

  if(verbose == 1 | verbose == 2)
    print("Started building SETAR-Tree")

  for(d in 1:depth){
    if(verbose == 1 | verbose == 2)
      print(paste0("Depth: ", d))

    level_th_lags <- NULL
    level_thresholds <- NULL
    level_nodes <- list()
    level_significant_node_count <- 0
    level_split_info <- NULL

    for(n in 1:length(node_data)){
      if(verbose == 2)
        print(paste0("Node ", n))

      best_cost <- Inf
      th <- NULL
      th_lag <- NULL

      if((nrow(node_data[[n]]) >  (2 * ncol(data) + 2)) & split_info[n] == 1){

        for(lg in 1:ncol(data)){
          # print(paste0("Lag ", lg))

          # Finding the optimal lag and threshold that should be used for splitting
          # Optimised grid search
          ss_output <- find.cut.point(as.matrix(node_data[[n]][,-1]), as.matrix(node_data[[n]][,1]), node_data[[n]][,lg+1], start.con$nTh)
          cost <- ss_output[["RSS.left"]] + ss_output[["RSS.right"]]
          recheck <- ss_output[["need_recheck"]]

          if(recheck > round(start.con$nTh*0.6)){
            # If optimised grid search fails, then try with SS()
            ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values of the corresponding lag

            for(ids in 1:length(ths)){
              cost <- SS(ths[ids], node_data[[n]], lg)

              if(cost <= best_cost) { # Find th and th_lag which minimizes the squared errors
                best_cost <- cost
                th <- ths[ids]
                th_lag <- lg
              }
            }
          }else{
            if(cost <= best_cost) { # Find th and th_lag which minimize the squared errors
              best_cost <- cost
              th <- ss_output[["cut.point"]]
              th_lag <- lg
            }
          }
        }

        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th) # Get the child nodes

          # Check whether making the split is worth enough
          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, significance, verbose)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_data[[n]], splited_nodes, error_threshold, verbose)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, significance, verbose) & check_error_improvement(node_data[[n]], splited_nodes, error_threshold, verbose)

          if(is_significant){ # Split the node into child nodes only if making that split is worth enough
            level_th_lags <- c(level_th_lags, th_lag)
            level_thresholds <- c(level_thresholds, th)
            level_split_info <- c(level_split_info, rep(1, 2))
            level_significant_node_count <- level_significant_node_count + 1

            for(s in 1:length(splited_nodes)){
              len <- length(level_nodes)
              level_nodes[[len + 1]] <- splited_nodes[[s]]
            }
          }else{
            level_th_lags <- c(level_th_lags, 0)
            level_thresholds <- c(level_thresholds, 0)
            level_split_info <- c(level_split_info, 0)

            len <- length(level_nodes)
            level_nodes[[len + 1]] <- node_data[[n]]
          }
        }else{
          level_th_lags <- c(level_th_lags, 0)
          level_thresholds <- c(level_thresholds, 0)
          level_split_info <- c(level_split_info, 0)

          len <- length(level_nodes)
          level_nodes[[len + 1]] <- node_data[[n]]
        }
      }else{
        level_th_lags <- c(level_th_lags, 0)
        level_thresholds <- c(level_thresholds, 0)
        level_split_info <- c(level_split_info, 0)

        len <- length(level_nodes)
        level_nodes[[len + 1]] <- node_data[[n]]
      }
    }

    if(level_significant_node_count > 0){
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      split_info <- level_split_info
      significance <- significance/significance_divider # Defining the significance for the next level of the tree
    }else
      break # If all nodes in a particular tree level are not further split, then stop
  }


  # Model training
  # Check whether the tree has any nodes. If not, train a single pooled regression model
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    num_of_leaf_instances <- length(leaf_nodes)
    leaf_trained_models <- list()
    leaf_instance_dis <- NULL

    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]]
      leaf_instance_dis <- c(leaf_instance_dis, nrow(leaf_nodes[[ln]]))
    }

    coefficients <- names(leaf_trained_models[[1]]$coefficients)
  }else{
    leaf_trained_models <- fit_global_model(embed_data)[["model"]]
    num_of_leaf_instances <- 1
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
  output.tree$categorical_covariate_values <- categorical_covariates_unique_vals
  output.tree$input_type <- type
  output.tree$execution_time <- exec_time
  class(output.tree) <- "setartree"

  if(verbose == 1 | verbose == 2)
    print("Finished building SETAR-Tree")

  output.tree
}


# Function to fit a SETAR-Tree given a list of time series
fit.setartree.series <- function(time_series_list, lag = 10, depth = 1000, significance = 0.05, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", verbose = 2){

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

  fit.setartree.df(embedded_series[,-1], embedded_series[,1], depth, significance, significance_divider, error_threshold, stopping_criteria, "list", verbose)
}


# Predict method for SETAR-Tree fits
predict.setartree <- function(tree, newdata){
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

  predictions <- NULL

  if(tree$num_leaves > 1){
    if(nrow(newdata) > 1){
      for(r in 1:nrow(newdata)){
        leaf_index <- get_leaf_index(newdata[r,], tree$opt_lags, tree$opt_thresholds) # Identify the leaf node corresponding with the new data
        predictions <- c(predictions, predict.glm(object = all_leaf_models[[leaf_index]], newdata = newdata[r,]))
      }
    }else{
      leaf_index <- get_leaf_index(newdata, tree$opt_lags, tree$opt_thresholds) # Identify the leaf node corresponding with the new data
      predictions <- predict.glm(object = all_leaf_models[[leaf_index]], newdata = newdata)
    }
  }else{
    predictions <- predict.glm(object = all_leaf_models, newdata = newdata)
  }

  as.numeric(predictions)
}


# Function to obtain predictions for a given list of time series
predictseries.setartree <- function(tree, time_series_list, h = 5){

  final_lags <- NULL
  forecasts <- NULL
  lag <- tree$lag
  coefficient_names <- tree$coefficients

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
    horizon_predictions <- as.numeric(predict(tree, as.data.frame(final_lags)))
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
    current_result$method = "SETAR-Tree"
    current_result$mean = ts(as.numeric(forecasts[i,]), start = length(time_series)+1)
    current_result$x = ts(time_series)
    class(current_result) <- "forecast"
    results[[i]] <- current_result
  }

  results
}



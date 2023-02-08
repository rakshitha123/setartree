#' A list of 20 time series constructed by using the Chaotic Logistic Map (May, 1976) data generation process.
#' This is a part of a simulated dataset used in Hewamalage et al.(2021).
#' These series can be used to train the SETAR-Tree and SETAR-Forest models.
#'
#' @title Chaotic logistic map example time series
#' @name chaotic_logistic_series
#' @docType data
#' @format A list containing 20 numerical vectors.
#' @keywords datasets
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated dynamics. Nature, 261, 459–467.
#'
#' Hewamalage, H., Bergmeir, C., & Bandara, K. (2021). Global models for time series forecasting: A simulation study. Pattern Recognition, 108441.
#' @examples
#' chaotic_logistic_series
NULL


#' A dataframe containing 120 instances that can be used to train the SETAR-Tree and SETAR-Forest models.
#' The data are related to the number of hits or web traffic of a set of Wikipedia pages.
#' Each instance in the dataframe consists of 10 time series lags (Lag1 to Lag10), 3 categorical covariates (Project, Access and Agent) and the corresponding true outputs (y).
#' The data were downloaded from the Wikimedia REST API (Wikimedia, 2022).
#'
#' @title A dataframe of training instances
#' @name web_traffic_train
#' @docType data
#' @format A dataframe containing 120 training instances.
#' @keywords datasets
#' @references
#' Wikimedia Analytics Team (2022). Wikistats: Pageview complete dumps.\cr
#' URL https://dumps.wikimedia.org/other/pageview_complete
#' @examples
#' web_traffic_train
NULL


#' A dataframe containing 12 instances that can be used to test the SETAR-Tree and SETAR-Forest models.
#' The data are related to the number of hits or web traffic of a set of Wikipedia pages.
#' Each intance consists of 10 time series lags (Lag1 to Lag10) and 3 categorical covariates (Project, Access and Agent).
#' The data were downloaded from the Wikimedia REST API (Wikimedia, 2022).
#'
#' @title A dataframe of test instances
#' @name web_traffic_test
#' @docType data
#' @format A dataframe containing 12 test instances.
#' @keywords datasets
#' @references
#' Wikimedia Analytics Team (2022). Wikistats: Pageview complete dumps.\cr
#' URL https://dumps.wikimedia.org/other/pageview_complete
#' @examples
#' web_traffic_test
NULL

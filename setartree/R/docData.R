#' A list of 20 time series constructed by using the Chaotic Logistic Map (May, 1976) data generation process. 
#' This is a part of a simulated dataset used in Hewamalage et al.(2021). 
#' These series can be used to train the SETAR-Tree and SETAR-Forest models.
#' This is data from the original SNNS examples directory ported to R and stored as one list.
#' The function \code{\link{readPatFile}} was used to parse all pattern files (.pat) from the 
#' original SNNS examples directory. Due to limitations of that function, pattern files
#' containing patterns with variable size were omitted. 
#'
#' @title Chaotic logistic map example time series
#' @name chaotic_logistic_series
#' @docType data
#' @value A list containing 20 numerical vectors.
#' @keywords datasets
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated dynamics. Nature, 261, 459–467.
#' 
#' Hewamalage, H., Bergmeir, C., & Bandara, K. (2021). Global models for time series forecasting: A simulation study. Pattern #' Recognition, 108441.
#' @examples
#' data(chaotic_logistic_series)
#' names(chaotic_logistic_series)
NULL

#' TODO: Still add the other two datasets in the same format here. 
#' This is a part of a simulated dataset used in Hewamalage et al.(2021). 
#' These series can be used to train the SETAR-Tree and SETAR-Forest models.
#' This is data from the original SNNS examples directory ported to R and stored as one list.
#' The function \code{\link{readPatFile}} was used to parse all pattern files (.pat) from the 
#' original SNNS examples directory. Due to limitations of that function, pattern files
#' containing patterns with variable size were omitted. 
#'
#' @title TODO
#' @name web_traffic
#' @docType data
#' @value A list containing 20 numerical vectors.
#' @keywords datasets
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated dynamics. Nature, 261, 459–467.
#' 
#' Hewamalage, H., Bergmeir, C., & Bandara, K. (2021). Global models for time series forecasting: A simulation study. Pattern #' Recognition, 108441.
#' @examples
#' data(chaotic_logistic_series)
#' names(chaotic_logistic_series)
NULL


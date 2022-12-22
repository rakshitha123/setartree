\name{web_traffic_train}
\docType{data}
\alias{web_traffic_train}
\title{A Dataframe of Training Instances}
\description{
  A dataframe containing 120 instances that can be used to train the SETAR-Tree and SETAR-Forest models.
  The data are related to the number of hits or web traffic of a set of Wikipedia pages.
  Each intance in the dataframe consists of 10 time series lags (Lag1 to Lag10), 3 categorical covariates (Project, Access and Agent) and the corresponding true outputs (y).
  The data were downloaded from the \href{https://tinyurl.com/y9m447zp}{Wikimedia REST API} (Wikimedia, 2022).
}
\usage{web_traffic_train}
\format{A dataframe containing 120 training instances.}
\keyword{datasets}
\references{
Wikimedia Analytics Team (2022). Wikistats: Pageview complete dumps. \cr URL https://dumps.wikimedia.org/other/pageview_complete
}

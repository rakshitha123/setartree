\name{web_traffic_test}
\docType{data}
\alias{web_traffic_test}
\title{A Dataframe of Test Instances}
\description{
  A dataframe containing 12 instances that can be used to test the SETAR-Tree and SETAR-Forest models.
  The data are related to the number of hits or web traffic of a set of Wikipedia pages.
  Each intance consists of 10 time series lags (Lag1 to Lag10) and 3 categorical covariates (Project, Access and Agent).
  The data were downloaded from the \href{https://tinyurl.com/y9m447zp}{Wikimedia REST API} (Wikimedia, 2022).
}
\usage{web_traffic_test}
\format{A dataframe containing 12 test instances.}
\keyword{datasets}
\references{
Wikimedia Analytics Team (2022). Wikistats: Pageview complete dumps. \cr URL https://dumps.wikimedia.org/other/pageview_complete
}

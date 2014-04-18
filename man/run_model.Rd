\name{run_model}
\alias{run_model}
\title{Build a model using a data source from scratch.}
\usage{
run_model(key = get_cache("last_model") \%||\%
  getOption("syberia.default_model"), ..., fresh = FALSE, verbose = TRUE)
}
\arguments{
  \item{key}{a string or list. If the former, there must be
  a file with name \code{model_stages} followed by
  \code{.r} so that syberia can read the model
  configurations.}
}
\description{
Build a model using a data source from scratch.
}


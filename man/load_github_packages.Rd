\name{load_github_packages}
\alias{load_github_packages}
\title{Load packages from github as specified in the .github_packages
variable found in config.r.}
\usage{
load_github_packages(pkgs, silent = FALSE)
}
\arguments{
  \item{pkgs}{a list of packages that get passed as
  arguments into devtools::install_github}

  \item{silent}{determines whether to trigger an error if a
  package is not found.}
}
\value{
a vector of logicals corresponding to whether the packages
were
}
\description{
Load packages from github as specified in the
.github_packages variable found in config.r.
}


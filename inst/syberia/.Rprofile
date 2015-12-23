options(stringsAsFactors = FALSE)
library(methods); library(utils); library(stats);
options(menu.graphics = FALSE) # Disable tcl/tk for installation from CRAN
options(repos = structure(c(CRAN = "http://streaming.stat.iastate.edu/CRAN/")))
if (!"devtools" %in% utils::installed.packages()[,1]) install.packages("devtools")
if (!"bettertrace" %in% utils::installed.packages()[,1]) devtools::install_github('robertzk/bettertrace')
library(bettertrace);
if (!require(Ramd)) devtools::install_github("robertzk/Ramd")
Ramd::packages("magrittr", "RJSONIO", "lubridate", "plyr", "timeDate", "arules", "AUC", "base64enc", "pROC", "curl", "rjson")

manual_box <- function(pkg_name, version, github) {
  if (!nzchar(Sys.getenv("CI")) &&
      (!is.element(pkg_name, installed.packages()[, 1]) ||
       utils::packageVersion(pkg_name) < package_version(version))) {
    args <- list(github)
    if (!grepl("@", fixed = TRUE, github)) {
      args$ref <- version
    }
    do.call(devtools::install_github, args)
  }
}

invisible(Map(manual_box, c("testthatsomemore", "lockbox", "Rcpp", "R6", "purrr"),
    c("0.2.4", "0.1.10", "0.12.0", "2.1.1", "0.0.0.9000"),
    c("robertzk/testthatsomemore", "robertzk/lockbox", "RcppCore/Rcpp",
      "wch/R6@v2.1.1", "hadley/purrr@73f736a01eb99932f7ffb33094433afaea4038f9")))

if (packageVersion("roxygen2") != package_version("4.1.1")) {
  Ramd::packages("crayon")
  packageStartupMessage(crayon::green("Updating roxygen...\n"))
  devtools::install_github("klutometis/roxygen", ref = "v4.1.1")
}

options(lockbox.env = if (!nzchar(Sys.getenv("CI"))) "development" else "test")

if (!nzchar(Sys.getenv("R_ROOT"))) suppressPackageStartupMessages(library(lockbox))
Sys.setenv("R_ROOT" = "TRUE") # Don't re-lockbox for process forks, like GBM

# Calling syberia_project within ~/.Rprofile causes infinite loops, so we disable it temporarily.
assign("syberia_project", local({ calls <- list(); function(...) { calls <<- list(calls, list(...)) } }), envir = globalenv())
if (file.exists("~/.Rprofile")) source("~/.Rprofile")
invisible(local({
  syberia_project_calls <- environment(get("syberia_project", envir = globalenv()))$calls
  rm("syberia_project", envir = globalenv())
  lapply(syberia_project_calls, function(call) { do.call(syberia_project, call) })
}))

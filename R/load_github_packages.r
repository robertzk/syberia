#' Load packages from github
#'
#' @param pkgs a list of packages that get passed as arguments into
#'    devtools::install_github, or just a list or vector of names if
#'    \code{install = FALSE}
#' @param install a logical. Will fetch any required packages from github
#'    and install locally if necessary.
#' @param silent a logical. Determines whether to trigger an error if a package
#'    is not found.
#' @return a vector of logicals corresponding to whether the packages were
#' @seealso \code{\link{install_github_packages}}
#' @export
#' @examples
#' \dontrun{
#' load_github_packages(list(list('Ramd', 'robertzk'), list('bigrquery', 'hadley')))
#' load_github_packages(c('Ramd', 'bigrquery'), install = FALSE)
#' }
load_github_packages <- function(pkgs, install = TRUE, silent = FALSE) {
  package_names <-
    vapply(pkgs, function(pkg) pkg[[1]], character(1))
  if (install) {
    uninstalled_packages <-
      pkgs[vapply(package_names, Negate(package_exists), logical(1))]
    install_github_packages(uninstalled_packages)
  }

  vapply(package_names, function(pkg) {
    loaded <- suppressWarnings(require(pkg, character.only = TRUE))
    if (!loaded && !silent)
      stop("Failed to load package ", pkg, " from Github.")
    loaded
  }, logical(1))
}

#' Install packages from github
#'
#' @param pkgs a list of packages that get passed as arguments into
#'    devtools::install_github
#' @seealso \code{\link{load_github_packages}}
#' @export
#' @examples
#' \dontrun{
#' install_github_packages(list(list('Ramd', 'robertzk'),
#'   list('s3mpi', 'robertzk', 'rconference')))
#' }
install_github_packages <- function(pkgs) {
  for(pkg in pkgs) {
    pkg <- as.list(pkg)
    suppressMessages(do.call("install_github", pkg,
      envir = as.environment('package:devtools')))
  }
}

#' Load packages from github as specified in the .github_packages
#' variable found in config.r. 
#'
#' @param pkgs a list of packages that get passed as arguments into
#'    devtools::install_github
#' @param silent determines whether to trigger an error if a package is
#'    not found.
#' @return a vector of logicals corresponding to whether the packages were
#' @export
load_github_packages <- function(pkgs, silent = FALSE) {
  package_names <-
    vapply(pkgs, function(pkg) pkg[[1]], character(1))
  uninstalled_packages <-
    pkgs[vapply(package_names, Negate(package_exists), logical(1))]
  install_github_packages(uninstalled_packages)

  vapply(package_names, function(pkg) {
    loaded <- suppressWarnings(require(pkg, character.only = TRUE))
    if (!loaded && !silent)
      stop("Failed to load package ", pkg, " from Github.")
    loaded
  }, logical(1))
}

install_github_packages <- function(pkgs) {
  lapply(pkgs, function(pkg) {
    pkg <- as.list(pkg)
    do.call("install_github", pkg,
            envir = as.environment('package:devtools'))
  })
}

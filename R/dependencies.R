.github_packages <- list(
  list('syberiaStructure', 'robertzk'),
  list('syberiaStages', 'robertzk'),
 #list('productivus', 'robertzk'),
  list('Ramd', 'robertzk'),
 #list('frost', 'robertzk'),
  list('stagerunner', 'robertzk'),
  list('syberiaMungebits', 'robertzk'),
  list('mungebits', 'robertzk'),
  list('tundra', 'robertzk')
)

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

  suppressMessages(suppressWarnings(require(utils)))
  apply_method <-
    if ('pbapply' %in% base::.packages() && !identical(silent, FALSE)) {
      suppressMessages(suppressWarnings(require(pbapply)))
      pblapply
    } else lapply

  if (install) {
    uninstalled_packages <-
      pkgs[vapply(package_names, Negate(package_exists), logical(1))]
    # install_github_packages(uninstalled_packages, apply_method = apply_method)
  }

  apply_method(package_names, function(pkg) {
    loaded <- suppressMessages(suppressWarnings(require(pkg, character.only = TRUE)))
    if (!loaded && !silent)
      stop("Failed to load package ", pkg, " from Github.")
    loaded
  })
  TRUE
}

#' Install packages from github
#'
#' @param pkgs a list of packages that get passed as arguments into
#'    devtools::install_github
#' @param apply_method a function. Useful for custom progress decorators. The
#'    default is \code{lapply}.
#' @seealso \code{\link{load_github_packages}}
#' @export
#' @examples
#' \dontrun{
#' install_github_packages(list(list('Ramd', 'robertzk'),
#'   list('s3mpi', 'robertzk', 'rconference')))
#' }
install_github_packages <- function(pkgs, apply_method = lapply) {
  suppressMessages(suppressWarnings(require(stats))) # devtools needs this for stats::setNames
  suppressMessages(suppressWarnings(require(devtools))) 
  apply_method(pkgs, function(pkg) {
    pkg <- as.list(pkg)
    do.call("install_github", pkg,
      envir = as.environment('package:devtools'))
  })
}


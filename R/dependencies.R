.github_packages <- list(
  list('syberiaStructure', 'robertzk'),
  list('syberiaStages', 'robertzk'),
  list('productivus', 'robertzk'),
  list('Ramd', 'robertzk'),
  list('frost', 'robertzk'),
  list('stagerunner', 'robertzk'),
  list('mungebitsTransformations', 'robertzk'),
  list('mungebits', 'robertzk'),
  list('tundra', 'robertzk')
)

#' Fetch the configuration for a Syberia file. 
#'
#' @param root character. The root of the Syberia project. The
#'   default is \code{syberia_root()}.
#' @param exists_check logical. If \code{TRUE}, it will only return
#'   whether or not the directory contains a syberia configuration
#'   file (either \code{syberia.config} or \code{syberia_config.r},
#'   with the extensions case-insensensitive, .r or .R). The default
#'   is \code{FALSE}: The whole syberia configuration will be returned
#'   for this project as a list.
#' @return the syberia configuration. All local variables set in
#'   the syberia configuration file will be part of a list of options
#'   for this project. TODO: (RK) Record list of possible options somewhere.
#' @export
syberia_config <- function(root = syberia_root(), exists_check = FALSE) {
  files <- c('syberia.config', 'syberia_config.r', 'syberia_config.R')
  config_file <- NULL
  for (file in files)
    if (file.exists(tmp <- file.path(root, file))) config_file <- tmp
  if (exists_check) return(!is.null(config_file))
  if (is.null(config_file)) stop("No syberia config file at: ", root)
  provided_env <- new.env()
  provided_env$syberia_root <- root
  config_env <- new.env(parent = provided_env)
  source(config_file, local = config_env)
  as.list.environment(config_env)
}

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
    if ('pbapply' %in% utils::installed.packages() && !identical(silent, FALSE)) {
      suppressMessages(suppressWarnings(require(pbapply)))
      pblapply
    } else lapply

  if (install) {
    uninstalled_packages <-
      pkgs[vapply(package_names, Negate(package_exists), logical(1))]
    install_github_packages(uninstalled_packages, apply_method = apply_method)
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


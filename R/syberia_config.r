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
  config_env <- new.env()
  source(config_file, local = config_env)
  as.list.environment(config_env)
}


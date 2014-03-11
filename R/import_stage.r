#' Import data stage for Syberia model process.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param import_options a list. The available import options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
import_stage <- function(modelenv, import_options) {
  # By default, try loading from only one adapter
  if (!all(vapply(import_options, is.list, logical(1)))) {
    import_options$adapter <- import_options$adapter %||% 'file'
    import_options <-
      structure(list(import_options), .Names = import_options$adapter)
  }

  build_import_stagerunner(modelenv, import_options)

  if (is.character(import_options$skip) &&
      exists(tmp <- import_options$skip)) {
    modelenv$data <- get(tmp)
    modelenv$import_stage$file <- import_options$file
    return(NULL)
  }

  if (import_options$adapter == 's3') {
    require(s3mpi)
    modelenv$data <- s3read(import_options$file)
  } else {
    modelenv$data <- read.csv(import_options$file, stringsAsFactors = FALSE)
  }
  modelenv$import_stage$file <- import_options$file
  NULL
}

#' Build a stagerunner for importing data with backup sources.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param import_options a list. Nested list, one adapter per list entry.
build_import_stagerunner <- function(modelenv, import_options) {
  lapply(import_options, function() {

  })
}

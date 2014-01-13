#' Import data stage for Syberia model process.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param import_options a list. The available import options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
import_stage <- function(modelenv, import_options) {
  stopifnot('file' %in% names(import_options))

  import_options$adapter <- import_options$adapter %||% 'file'
  if (import_options$adapter == 's3') {
    require(s3mpi)
    modelenv$data <- s3read(import_options$file)
  } else {
    modelenv$data <- read.csv(import_options$file)
  }
  modelenv$import_stage$file <- import_options$file
  NULL
}

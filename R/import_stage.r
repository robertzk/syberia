#' Import data stage for Syberia model process.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param import_options a list. The available import options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
import_stage <- function(modelenv, import_options) {
  build_import_stagerunner(modelenv, normalize_import_options(import_options))
}

#' Normalize import options by converting a single option into a
#' list of one if necessary.
#'
#' @param import_options list. A list of import sources.
#' @return a normalized list of import options
normalize_import_options <- function(import_options) {
  # By default, try loading from only one adapter
  if (!all(vapply(import_options, is.list, logical(1)))) {
    if (is.character(import_options)) import_options <- list(file = import_options)
    import_options$adapter <- import_options$adapter %||% 'file'
    import_options <-
      structure(list(import_options), .Names = import_options$adapter)
  }
  import_options
}

#' Build a stagerunner for importing data with backup sources.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param import_options a list. Nested list, one adapter per list entry.
build_import_stagerunner <- function(modelenv, import_options) {
  stages <- lapply(seq_along(import_options), function(index) {
    stage <- function(modelenv) {
      # Only run if data isn't already loaded
      if (!'data' %in% ls(modelenv)) {
        attempt <- suppressWarnings(suppressMessages(
          tryCatch(fn(modelenv, opts), error = function(e) FALSE)))
      }
    }
    opts <- normalize_import_options(import_options[[index]])[[1]]
    adapter <- names(import_options)[index] %||% opts$adapter
    environment(stage)$fn <- import_adapter(adapter)
    environment(stage)$opts <- opts
    stage
  })

  stages[[length(stages) + 1]] <- function(modelenv) {
    if (!'data' %in% ls(modelenv))
      stop("Failed to load data from all data sources")
    modelenv$import_stage$variable_summaries <-
      statsUtils::variable_summaries(modelenv$data) 
  }
  stageRunner$new(modelenv, stages)
}

#' Fetch an import adapter.
#'
#' @param adapter character. Only supported so far are 's3' and 'file'.
#'    The default is 'file'.
import_adapter <- function(adapter = 'file') {
  stopifnot(is.character(adapter))
  adapter <- tolower(adapter)
  # TODO: Given the similarities, if most future adapters are similarly,
  # maybe generate these using an adapter_template
  if (adapter == 's3') {
    function(modelenv, opts) {
      require(s3mpi)
      if (is.character(opts)) opts <- list(file = opts)
      filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
      stopifnot(is.character(filename))
      modelenv$data <- s3read(filename)
      modelenv$import_stage$file <- filename
    }
  } else {
    function(modelenv, opts) {
      if (is.character(opts)) opts <- list(file = opts)
      filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
      stopifnot(is.character(filename))
      modelenv$data <- read.csv(filename, stringsAsFactors = FALSE)
      modelenv$import_stage$file <- filename
    }
  }
}


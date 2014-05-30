#' Export stage for Syberia.
#' 
#' Precise behavior depends on adapter.
#'
#' @param export_options a list. The available export options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
export_stage <- function(export_options) {
  reserved_words <- c('copy')

  meta_options <- export_options[reserved_words]
  if (!is.null(tmpnames <- names(export_options)))
    export_options <- export_options[setdiff(tmpnames, reserved_words)]
  
  # By default, try writing to only one adapter
  if (!all(vapply(export_options, is.list, logical(1)))) {
    export_options$adapter <- export_options$adapter %||% 'file'
    export_options <-
      structure(list(export_options), .Names = export_options$adapter)
  }

  build_export_stagerunner(normalize_export_options(export_options), meta_options)
}

#' Normalize export options by converting a single option into a
#' list of one if necessary.
#'
#' @param export_options list. A list of export sources.
#' @return a normalized list of export options
normalize_export_options <- function(export_options) {
  # By default, try saving to only one adapter
  if (!all(vapply(export_options, is.list, logical(1)))) {
    if (is.character(export_options)) export_options <- list(file = export_options)
    export_options$adapter <- export_options$adapter %||% 'file'
    export_options <-
      structure(list(export_options), .Names = export_options$adapter)
  }
  export_options
}


#' Build a stagerunner for exporting data with backup sources.
#'
#' @param export a list. Nested list, one adapter per list entry.
build_export_stagerunner <- function(export_options, meta_options = list()) {
  stages <- lapply(seq_along(export_options), function(index) {
    stage <- function(modelenv) {
      attempt <- suppressWarnings(suppressMessages(
        tryCatch(fn(modelenv, opts), error = function(e) FALSE)))
    }
    opts <- normalize_export_options(export_options[[index]])[[1]]
    adapter <- names(export_options)[index] %||% opts$adapter 
    environment(stage)$fn <- export_adapter(adapter)
    environment(stage)$opts <- opts
    environment(stage)$adapter <- adapter
    stage
  })
  names(stages) <-
    vapply(stages, function(stage)
                     paste0("Export to ", environment(stage)$adapter),
           character(1))

  if ('copy' %in% names(meta_options)) {
    stages <- append(list("(Internal) Copy model to a global variable" = function(modelenv) {
      stopifnot(is.character(meta_options$copy))
      assign(meta_options$copy, modelenv$model_stage$model, globalenv())  
      #copy is assigned to the global environment which is a local copy of the trained model
    }), stages)
  }

  stages
}

#' Fetch an export adapter.
#'
#' @param adapter character. Only supported so far are 's3' and 'file'.
#'    The default is 'file'.
#' @param opts list. The options that get passed to the export adapter.
export_adapter <- function(adapter = 'file') {
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
      #capture.output(s3store(modelenv$model_stage$model, export_options$file))
      s3store(modelenv$model_stage$model, filename)
      modelenv$export_stage$file <- c(modelenv$export_stage$file, filename)
    }
  } else {
    function(modelenv, opts) {
      if (is.character(opts)) opts <- list(file = opts)
      filename <- opts$file %||% opts$filename %||% opts$name %||% opts$path
      stopifnot(is.character(filename))
      saveRDS(modelenv$model_stage$model, filename)
      modelenv$export_stage$file <- c(modelenv$export_stage$file, filename)
    }
  }
}


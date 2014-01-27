#' Export stage for Syberia.
#' 
#' Precise behavior depends on adapter.
#'
#' @param modelenv an environment. The current modeling environment.
#' @param export_options a list. The available export options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
export_stage <- function(modelenv, export_options) {
  export_options$adapter <- export_options$adapter %||% 'file'
  if (export_options$adapter == 's3') {
    require(s3mpi)
    stopifnot('file' %in% names(export_options))
    capture.output(s3store(modelenv$model_stage$model, export_options$file))
    NULL
  } else {
    stopifnot('file' %in% names(export_options))
    saveRDS(modelenv$model_stage$model, export_options$file)
  }

  if ('copy' %in% names(export_options$copy)) {
    stopifnot(is.character(export_options$copy))
    assign(export_options$copy, modelenv$model_stage$model, globalenv())
  }
  NULL
}

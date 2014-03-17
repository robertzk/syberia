#' Build a model using a data source from scratch.
#' 
#' @param key a string or list. If the former, there must be a
#'   file with name \code{model_stages} followed by \code{.r} so that syberia
#'   can read the model configurations.
#' @export
run_model <- function(key = get_cache('last_model') %||% getOption('syberia.default_model'),
                      ..., verbose = TRUE) {
  # TODO: Add path mechanism
  
  model_stages <- 
    if (missing(key) && is.stagerunner(tmp <- active_runner())) tmp
    else if (is.character(key)) {
      if (FALSE == (src_file <- normalized_filename(key)))
        stop(pp("No file for model '#{key}'"))
      source(src_file)$value
    }
    else if (is.list(key)) key
    else if (is.stagerunner(key)) key
    else stop("Invalid model key")

  set_cache(key, 'last_model')

  stagerunner <- construct_stage_runner(model_stages)
  set_cache(stagerunner, 'last_stagerunner')
  out <- tryCatch(stagerunner$run(..., verbose = verbose),
           error = function(e) NULL)
  if (is.null(out)) invisible(stagerunner) else out
}


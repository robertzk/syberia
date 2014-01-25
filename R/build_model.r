#' Build a model using a data source from scratch.
#' @param key a string or list. If the former, there must be a
#'   file with name \code{model_stages} followed by \code{.r} so that syberia
#'   can read the model configurations.
#' @export
build_model <- function(key = get_cache('last_model') %||% getOption('syberia.default_model')) {
  # TODO: Add path mechanism
  
  model_stages <- 
    if (is.character(key)) {
      if (FALSE == (src_file <- normalized_filename(key)))
        stop(pp("No file for model '#{key}'"))
      source(src_file)$value
    }
    else if (is.list(key)) key
    else stop("Invalid model_stages argument")

  set_cache(key, 'last_model')
  stage_runner(model_stages)
}



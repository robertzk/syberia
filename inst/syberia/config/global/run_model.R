#' Build a model using a data source from scratch.
#'
#' TODO: (RK) Document this method more.
#'
#' @param key a string or list. If the former, there must be a
#'   file with name \code{model_stages} followed by \code{.r} so that syberia
#'   can read the model configurations.
#' @param ... additional arguments to pass to \code{$run(...)} on the stageRunner.
#'   For example, \code{to = 'some/key'}.
#' @param fresh logical. Whether or not to use the cache. By default, \code{FALSE}.
#' @param verbose logical. Whether or not to display messages. The default is
#'   \code{TRUE}.
#' @export
run_model <- function(director)
  function(key = director$.cache$last_key, ..., fresh = FALSE, verbose = TRUE) {
    if (missing(key)) { key <- director$.cache$last_key }
    if (!is.character(key)) { stop("No model executed.", call. = FALSE) }
    director$.cache$last_key <- key

    keys <- director$find(key, base = 'models/')
    if (length(keys) == 0) {
      stop("No model ", sQuote(key),
           " found in syberia project ", sQuote(root()), call. = FALSE)
    }

    # Construct the stageRunner and then execute it.
    output <- director$resource(keys[1])$value()$run(..., verbose = verbose)
    director$.cache$last_run <- output
    output
  }

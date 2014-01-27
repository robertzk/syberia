#' Run all stages on a model
#'
#' @param stages a list of lists. Each sublist is the argument to its
#'    named stage.
#' @export
stage_runner <- function(stages) {
  stopifnot(is.list(stages))
  if ("" %in% names(stages) || is.null(names(stages)))
    stop("All model steps must be named (e.g., import, data, model, ...).")

  modelenv <- new.env()
  for (stage_name in names(stages)) {
    stage_var <- pp('#{stage_name}_stage')
    if (!exists(stage_var))
      stop("No such stage '", stage_name, "'")
    # TODO: Check for is.function ?

    modelenv[[stage_var]] <- list()
    cat(pp("Beginning #{pp(testthat:::colourise(stage_name, 'green'), ' stage')}...\n"))
    get(stage_var)(modelenv, stages[[stage_name]])
    cat(pp("Done with #{pp(testthat:::colourise(stage_name, 'blue'), ' stage')}...\n"))
  }
}


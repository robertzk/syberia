#' Return a stageRunner object that parametrizes a list of stages.
#'
#' Each stage is first fed through a function that converts it to a stageRunner
#' or a function. For example, list(import = X, ...) gets converted by,
#' amongst other things, looking for a variable import_stage and passing
#' in X. 
#'
#' This kind of procedure allows one to define new stage types dynamically,
#' and "reach in" and execute only sub-parts of stages by using stageRunner
#' effectively.
#'
#' @param stages a list of lists. Each sublist is the argument to its
#'    named stage.
#' @return stageRunner parametrizing the given stages
#' @export
construct_stage_runner <- function(stages) {
  if (is.stagerunner(stages)) return(stages)

  stopifnot(is.list(stages))
  if ("" %in% names(stages) || is.null(names(stages)))
    stop("All model steps must be named (e.g., import, data, model, ...).")

  modelenv <- new.env()
  stages <- structure(lapply(names(stages), function(stage_name) {
    stage_var <- paste0(stage_name, '_stage')
    if (!exists(stage_var)) stop("No such stage '", stage_name, "'")
    stage <- get(stage_var)
    stopifnot(is.function(stage))
    
    stage(modelenv, stages[[stage_name]])
  }), .Names = names(stages))

  stageRunner$new(modelenv, stages, remember = TRUE)
}



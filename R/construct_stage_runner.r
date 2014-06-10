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
  syberiaStructure:::syberia_stack(all = TRUE) # Clear the resource stack
  stages <- structure(lapply(seq_along(stages), function(stage_index) {
    stage_name <- names(stages)[stage_index]

    stage_var <- paste0(stage_name, '_stage')
    if (!exists(stage_var)) stop("No such stage '", stage_name, "'")
    stage <- get(stage_var)
    # TODO: (RK) Make this a just-in-time resource so users can define
    # their own stages.
    stopifnot(is.function(stage))
    
    arity <- length(formals(stage))
    if (arity > 1) stage(modelenv, stages[[stage_index]])
    else if (arity == 1) {
      # If the stage only takes one argument, Syberia adopts the convention
      # that if the argument contains the string 'opt' or 'par', then the
      # options get passed, otherwise the modeling environment does.
      # This allows for flexibility with the stage definitions.
      if (identical(TRUE, grepl('opt|par', names(formals(stage)))))
        stage(stages[[stage_index]])
      else stage(modelenv)
    } else stage()
  }), .Names = names(stages))

  # Remember any just-in-time resources that were compiled from running
  # these stages (for example, things in lib).
  model_resources <- syberiaStructure:::get_cache('model_resources')
  model_resources[[syberiaStructure:::get_cache('last_src_file')]] <-
    syberiaStructure:::syberia_stack(all = TRUE)
  syberiaStructure:::set_cache(model_resources, 'model_resources')

  # Label stages appropriately
  #names(stages) <- paste(vapply(stages, function(stage)
  #  if (is.stagerunner(stage)) "Begin" else "Run", character(1)),
  #  names(stages), "stage")

  # Recall the parameter remember :
  # whether or not it caches intermediate stages (placing it in cached_env
  # in the terminal stageRunnerNodes)
  stageRunner$new(modelenv, stages, remember = TRUE) 
}


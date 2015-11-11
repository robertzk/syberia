#' Determine fresh modeling environment.
#'
#' Modeling environments can either be a normal R \code{environment}
#' object or a \code{\link[objectdiff]{tracked_environment}} from the
#' objectdiff package. The global option syberia.environment_type
#' can be either \code{"environment"} or \code{"tracked_environment"}.
#' The default is to use a \code{"tracked_environment"}, since it offers
#' approximately the same speed for much less memory overhead.
#'
#' @return \code{new.env()} or \code{tracked_environment()} according as
#'   \code{getOption('syberia.environment_type')} is
#'   \code{"environment"} or \code{"tracked_environment"}, respectively,
#'   or the latter by default.
model_env <- function() {
  if (identical(getOption("syberia.environment_type"), "environment")) {
    new.env()
  } else {
    objectdiff::tracked_environment()
  }
}

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
#' @param modelenv environment. The modeling environment with which to
#'    construct the stagerunner. All operations will be performed on this
#'    environment. The default is \code{model_env()} (see above).
#' @param model_version character. The version of the model. Used for
#'    injecting in the model_card stage.
#' @return stageRunner parametrizing the given stages
#' @export
construct_stage_runner <- function(resource) {
  function(stages, model_version, modelenv = model_env()) {
    if (is.stagerunner(stages)) return(stages)

    stopifnot(is.list(stages))

    # Prepopulated constants.
    modelenv$model_version <- model_version

    if (NA %in% names(stages) || "" %in% names(stages) || is.null(names(stages))) {
      stop("All model steps must be named (e.g., import, data, model, ...).", call. = FALSE)
    }

    # TODO: (RK) Remove this hack. You shouldn't be running data import on CI.
    if (nzchar(Sys.getenv("CI"))) { stages$import <- NULL }

    stages <- structure(lapply(seq_along(stages), function(stage_index) {
      stage_name <- names(stages)[stage_index]

      stage <- resource(file.path('lib', 'stages', stage_name))
      # TODO: (RK) Expose more director methods to common resources so we can
      # check for existence and replace with a custom message? (of course we
      # can always tryCatch and look for "Cannot find resource")
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

    # Recall the parameter remember :
    # whether or not it caches intermediate stages (placing it in cached_env
    # in the terminal stageRunnerNodes)
    runner <- stageRunner$new(modelenv, stages, remember = TRUE)

    # TODO: (RK) Get rid of this awful hack to normalize a nested stageRunner.
    set_env <- function(x) {
      if (is.stagerunner(x)) {
        x$.context <- modelenv
        for (i in seq_along(x$stages)) set_env(x$stages[[i]])
      }
      else if (is(x, 'stageRunnerNode')) {
        x$.cached_env <- NULL
        x$.context <- modelenv
        x$executed <- FALSE
      }
    }
    set_env(runner)
    first_leaf <- stagerunner:::treeSkeleton$new(runner)$first_leaf()$object
    first_leaf$.cached_env <- new.env()
    stagerunner:::copy_env(first_leaf$.cached_env, modelenv)
    runner$.set_parents()

    runner
  }
}

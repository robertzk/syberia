attach_monitors <- function(stagerunner) {
  if (!exists('monitor', envir = globalenv(), inherits = FALSE)) return()
  monitors <- get('monitor', envir = topenv())
  if (is.function(monitors)) monitors <- list(monitors)
  else if (!is.character(monitors)) {
    warning("Global variable ", sQuote('monitor'), " exists but is not of ",
            "type character. You should probably give the name of the monitor(s).")
    return()
  } else {
    monitors <- lapply(monitors,
      function(monitor) resource(file.path('lib', 'debug', 'monitors', monitor)))
  }

  for (monitor in monitors) {
    # TODO: (RK) monitor nested stages
    # The function below has side-effects on the stagerunners, so we
    # don't need to return anything in this function.
    stagerunner$around(replicate(length(stagerunner$stages), monitor))
  }
}

#' Data stage for syberia models
#'
#' TODO: Document this more
#'
#' @param modelenv an environment. The persistent modeling environment.
#' @param munge_procedure a list. A list of mungepiece arguments,
#'    first preprocessed then passed to munge.
#' @param remember logical. Whether or not to use a caching stageRunner.
#'    The default is \code{TRUE}.
#' @export
data_stage <- function(modelenv, munge_procedure, remember = TRUE) {
  # preprocess_munge_procedure(munge_procedure)
  removed_steps <- vapply(munge_procedure, function(x) is(x, 'trigger'), logical(1))
  # TODO: sameAs/importFrom and butWith/except triggers
  # TODO: save trigger

  stagerunner <- munge(modelenv, munge_procedure,
    stagerunner = list(remember = remember),
    train_only = TRUE # This refers to us not wanting to set the mungebits
                      # as "trained" when running this stageRunner, since we
                      # would like to be able to run them multiple times --
                      # the stageRunner used in the main syberia model run is
                      # only used for training.
  )

  attach_monitors(stagerunner)
  stagerunner
}

#' Data stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param modelenv an environment. The persistent modeling environment.
#' @param munge_procedure a list. A list of mungepiece arguments,
#'    first preprocessed then passed to munge.
#' @export
data_stage <- function(modelenv, munge_procedure) {
  # preprocess_munge_procedure(munge_procedure)
  removed_steps <- vapply(munge_procedure, is.trigger, logical(1))
  # TODO: sameAs/importFrom and butWith/except triggers
  # TODO: save trigger

  stagerunner <- munge(modelenv, munge_procedure,
    stagerunner = list(remember = TRUE),
    train_only = TRUE # This refers to us not wanting to set the mungebits
                      # as "trained" when running this stageRunner, since we
                      # would like to be able to run them multiple times -- 
                      # the stageRunner used in the main syberia model run is
                      # only used for training.
  ) 
    
  stagerunner
}


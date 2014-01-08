#' Data stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param modelenv an environment. The persistent modeling environment.
#' @param munge_procedure a list. A list of mungepiece arguments,
#'    first preprocessed then passed to munge.
#' @export
data_stage <- function(modelenv, munge_procedure) {
  removed_steps <- c()
  for (i in seq_along(munge_procedure)) {
    # Check if we are storing the munging performed so far back into the file
    if (is_save_trigger(munge_procedure[[i]])) {
      # TODO: If this was already done, remove previous munge operations
      # and this one so we "fast forward". Not sure yet how this should be
      # detected.
      munge_procedure[[i]] <- list(list(eval(bquote(function(dataframe) {
        write.csv(dataframe, .(modelenv$import_stage$file))
      })), NULL)) # the NULL is to ensure this won't fire on prediction
      removed_steps <- c(removed_steps, i)
    }

    # TODO: sameAs/importFrom and butWith/except triggers

  }

  # Now okay to run munging procedure
  modelenv$data <- munge(modelenv$data, munge_procedure)
  if (length(removed_steps) > 0)
    attr(modelenv$data, 'mungepieces')[removed_steps] <- NULL
  NULL
}

is_save_trigger <- function(mungestep) {
  (is.character(mungestep) && tolower(mungestep) == 'save') ||
  (is.function(mungestep) && body(mungestep) == body(save))
}


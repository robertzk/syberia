#' Model stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param modelenv an environment. The persistent modeling environment.
#' @param model_parameters a list. Model-specific parameters, with the first
#'    parameter always being the model keyword for the tundra container
#'    (e.g., glm, gbm, etc.)
#' @export
model_stage <- function(modelenv, model_parameters) {
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  if (!exists(model_fn <- pp('tundra_#{model_parameters[[1]]}')))
    stop("Missing tundra container for keyword '", model_parameters[[1]], "'")

  # Remove the model keyword (e.g., "gbm", "glm", etc.)
  model_parameters[[1]] <- NULL

  # Instantiate tundra container for model
  
  modelenv$model_stage$model <-
    get(model_fn)(list(), model_parameters)

  # Train the model
  modelenv$model_stage$model$train(modelenv$data)

  # Manually skip munge procedure since it was already done
  modelenv$model_stage$model$munge_procedure <- attr(modelenv$data, 'mungepieces')
  out_model <<- modelenv$model_stage$model

  NULL
}




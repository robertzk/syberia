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
  stopifnot(is.character(model_parameters[[1]]))
  if (!exists(pp(model_fn <- 'tundra_#{model_parameters[[1]]'))
    stop("Missing tundra container for keyword '", model_parameters[[1]], "'")

  model_parameters[[1]] <- NULL
  # Instantiate tundra container for model
  modelenv$model_stage$model <-
    get(model_fn)(model_parameters, attr(modelenv$data, 'mungepieces'))

  # Train the model
  modelenv$model_stage$model$train(modelenv$data)
  NULL
}




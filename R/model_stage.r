#' Model stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param model_parameters a list. Model-specific parameters, with the first
#'    parameter always being the model keyword for the tundra container
#'    (e.g., glm, gbm, etc.)
#' @export
model_stage <- function(model_parameters) {
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  model_fn <- fetch_model_container(model_parameters[[1]])

  # Remove the model keyword (e.g., "gbm", "glm", etc.)
  model_parameters[[1]] <- NULL

  function(modelenv) {
    # Track variable summaries
    summaries <- modelenv$import_stage$variable_summaries
    summaries <- lapply(summaries,
      function(vars) vars[intersect(names(vars), colnames(modelenv$data))]
    )
    # TODO: Remove unimportant variables so they do not trigger
    # velocity check. For this, we need a model-agnostic variable
    # importance measure. Maybe add a hack for GBM first.

    # Instantiate tundra container for model
    modelenv$model_stage$model <-
      model_fn(list(), model_parameters, list(variable_summaries = summaries))

    # Train the model
    modelenv$model_stage$model$train(modelenv$data, verbose = TRUE)
    
    # Manually skip munge procedure since it was already done
    modelenv$model_stage$model$munge_procedure <-
      attr(modelenv$data, 'mungepieces') %||% list()
    # Since munge was called with train_only, the mungebits are incapable of
    # getting predicted. The line below remedies this.
    for (ix in seq_along(modelenv$model_stage$model$munge_procedure))
      modelenv$model_stage$model$munge_procedure[[ix]]$bit$trained <- TRUE
  }
}


#' Fetch a tundra model container.
#'
#' The container is fetched either from the tundra package or from
#' the \code{lib/classifiers} directory in the syberia project.
#'
#' If there is no tundra model associated to the keyword (like 'gbm'
#' or 'regularization'), you must place a function called \code{train}
#' and a function called \code{predict} in the an R file with the same
#' name as the keyword for your classification. For example,
#' if you are implementing least-angle regression, you could define
#' \code{lib/classifiers/lar.R} and from your syberia model use
#' \code{'lar'} for your model keyword.
#'
#' @param type character. The model keyword. This function will attempt
#'   to fetch the associated container construction function (with
#'   parameters \code{munge_procedure}, \code{default_args}, and
#'   \code{internal} from either (1) the tundra package, or (2)
#'   your syberia project's \code{lib/classifiers} directory (see
#'   description).
#' @return A container construction function (with
#'   parameters \code{munge_procedure}, \code{default_args}, and
#'   \code{internal} which takes these and return a \code{tundra_container}
#'   object with a \code{train} and \code{predict} method.
#' @export
fetch_model_container <- function(type) {
  # TODO: (RK) Should we be using syberia_objects for this?
  prefilename <- file.path(syberia_root(), 'lib', 'classifiers', type)
  if (!(file.exists(filename <- pp('#{prefilename}.r')) ||
        file.exists(filename <- pp('#{prefilename}.R')))) {
    if (exists(model_fn <- pp('tundra_#{type}'))) return(get(model_fn))
    stop("Missing tundra container for keyword '", type, "'. ",
         "It must exist in the tundra package or be present in ",
         pp("lib/classifiers/#{type}.R"), call. = FALSE)
  }
  
  provided_env <- new.env()
  source(filename, local = provided_env)
  if (!all(c('train', 'predict') %in% ls(provided_env)) &&
       is.function(provided_env$train) && is.function(provided_env$predict))
    stop(pp("The file lib/classifiers/#{type}.R must contain both ",
            "'train' and 'predict' functions."), call. = FALSE)

  function(munge_procedure = list(), default_args = list(), internal = list()) {
    tundra:::tundra_container$new(type, provided_env$train, provided_env$predict,
                                  munge_procedure, default_args, internal)
  }
}


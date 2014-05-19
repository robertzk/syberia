#' Build a model using a data source from scratch.
#' 
#' @param key a string or list. If the former, there must be a
#'   file with name \code{model_stages} followed by \code{.r} so that syberia
#'   can read the model configurations.
#' @export
run_model <- function(key = syberiaStructure:::get_cache('last_model') %||%
                      getOption('syberia.default_model'),
                      ..., fresh = FALSE, verbose = TRUE) {
  src_file <- NULL
  root <- NULL

  syberiaStructure:::set_cache(TRUE, 'runtime/executing')
  syberiaStructure:::set_cache(FALSE, 'runtime/any_modified')
  on.exit(syberiaStructure:::set_cache(FALSE, 'runtime/executing'))

  # Used by syberiaStructure::syberia_resource
  syberiaStructure:::set_cache(parent.frame(), 'runtime/current_env')

  model_stages <- 
    #if (missing(key) && is.stagerunner(tmp <- active_runner())) tmp
    #if (missing(key)) get_cache('last_model')
    if (is.character(key)) {
      if (FALSE == (src_file <- normalized_filename(key))) {
        root <- tryCatch(syberia_root(key), error = function(e) NULL) %||% syberia_root()
        src_file <- syberia_models(pattern = key, root = root)[1]
        if (is.null(src_file) || is.na(src_file) || identical(src_file, FALSE))
          stop(pp("No file for model '#{key}'"))
      } else root <- syberia_root(src_file) # Cache syberia root
      message("Loading model: ", src_file)
      model_filepath <- file.path(root %||% syberia_root(), 'models', src_file)
      resource <- syberia_resource_with_modification_tracking(model_filepath, root, body = FALSE)
      resource_dir <- dirname(model_filepath)
      if (basename(resource_dir) ==
          substring(tmp <- basename(model_filepath), 1, nchar(tmp) - 2)) {
        # If the model is in its own subdirectory, check all helpers files for
        # modification time as well.
        helper_files <- list.files(resource_dir, recursive = TRUE)
        # Trigger syberia_resource_with_modification_tracking to update whether
        # or not any helper files were modified.
        for (file in helper_files) syberia_resource_with_modification_tracking(
          file.path(resource_dir, file), root, body = FALSE)
      }
      resource$value()
    }
    else if (is.list(key)) key
    else if (is.stagerunner(key)) key
    else stop("Invalid model key")
  
  if (is.null(src_file))
    src_file <- syberiaStructure:::get_cache('last_model')
  if (is.null(root)) root <- syberia_root(src_file)

  display_file <- src_file
  src_file <- file.path(root, 'models', src_file)

  syberiaStructure:::set_cache(display_file, 'last_model')

  # TODO: Figure out how to integrate tests into this. We need something like:
  tests_file <- file.path(root, 'models', gsub('^[^/]+', 'test', display_file))
  testrunner <- NULL
  if (file.exists(tests_file)) {
    resource <- syberia_resource_with_modification_tracking(tests_file, root, body = FALSE)
    tests <- resource$value()
    testrunner <- stageRunner$new(new.env(), tests)
    testrunner$transform(function(fn) {
      require(testthat)
      force(fn)
      function(after) fn(cached_env, after)
    })
  }

  # Determine whether any of the files related to the model have changed
  # TODO: (RK) Use cleverer overloading of "source" later for this.

  # Coalesce the stagerunner if model file updated
  coalesce_stagerunner <- 
    (#missing(key) && is.character(key) && # TODO: (RK) Figure out if this is necessary
      is.character(tmp <- syberiaStructure:::get_cache('last_model')) &&
      identical(display_file, tmp) &&
      syberiaStructure:::get_cache('runtime/any_modified')) 

  if (coalesce_stagerunner) {
    stagerunner <- construct_stage_runner(model_stages)
    stagerunner$coalesce(syberiaStructure:::get_cache('last_stagerunner'))
  } else if (!missing(key) || !is.stagerunner(
      stagerunner <- syberiaStructure:::get_cache('last_stagerunner'))) {
    stagerunner <- construct_stage_runner(model_stages)
  }
  # TODO: (RK) Figure out a better wrapping mechanism for this
  if (!is.null(testrunner)) stagerunner$overlay(testrunner, 'tests')

  message("Running model: ", display_file)
  out <- tryCatch(stagerunner$run(..., verbose = verbose),
           error = function(e) e)
  # TODO: (RK) Attempt to record stack trace using evaluate:::try_capture_stack
  syberiaStructure:::set_cache(stagerunner, 'last_stagerunner')

  if (inherits(out, 'simpleError'))
    stop(out$message)
  else out
}


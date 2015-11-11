# Use local = TRUE when sourcing construct_stage_runner in order to ensure
# objectdiff package is loaded for stageRunner creation (with tracked_environments).
construct_stage_runner <- Ramd::define('construct_stage_runner')[[1]](resource)

preprocessor <- Ramd::define('preprocessor')[[1]]

# The models controller:
#
# Convert a model into a stagerunner.
function(args, resource, output, director, any_dependencies_modified) {
  # Support objectdiff::ls behavior.
  parent.env(parent.env(environment(construct_stage_runner))) <- environment()

  if (is.element("raw", names(args))) return(output)
  require(objectdiff)

  message("Loading model: ", resource)

  tests <- file.path('test', resource)
  has_tests <- director$exists(tests)
  has_tests <- FALSE
  if (has_tests) {
    # TODO: (RK) Better sanity checking?
    testrunner <- stageRunner$new(new.env(), director$resource(tests))
    testrunner$transform(function(fn) {
      library(testthat); force(fn)
      function(after) fn(cached_env, after)
    }) # TODO: (RK) Before/after only tests?
  }

  model_version <- gsub("^\\w+/", "", resource)
  if (isTRUE(args$fresh) || !identical(resource, director$cache_get("last_model"))) {
    stagerunner <- construct_stage_runner(output, model_version)
  } else if (any_dependencies_modified) {
    message(crayon::yellow("Copying cached environments..."))
    stagerunner <- construct_stage_runner(output, model_version)
    stagerunner$coalesce(director$cache_get("last_model_runner"))
  } else if (!director$cache_exists("last_model_runner")) {
    stagerunner <- construct_stage_runner(output, model_version)
  } else {
    stagerunner <- director$cache_get("last_model_runner")
  }

  if (has_tests) stagerunner$overlay(testrunner, "tests", flat = TRUE)

  director$cache_set("last_model", resource)
  director$cache_set("last_model_runner", stagerunner)

  stagerunner
}

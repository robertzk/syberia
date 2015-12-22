#' Run all tests in a syberia project or engine.
#'
#' The tests that will be run are all those in the \code{test} subdirectory
#' of the root of the syberia engine, unless otherwise specified.
#'
#' It is possible to introduce additional behavior prior to and after tests.
#' This can be used to perform additional testing not covered by sourcing
#' all files in the "test/" directory of the syberia engine.
#'
#' To provide a setup or teardown hook, simply place a function or list of
#' functions in a local variable \code{setup} or \code{teardown}, respectively,
#' in \code{config/environments/test} relative to the root of the syberia engine,
#' or pass the relevant \code{setup} or \code{teardown} parameters to this function.
#'
#' For example, creating a file \code{config/environments/test.R} with the code
#' \code{setup <- function(env) cat("Running all tests.")} will print a message
#' before all the tests are run. The one parameter the function must take is an
#' environment which will contain a single key, \code{director}, pointing to the 
#' object returned by calling \code{\link{syberia_engine}}.
#'
#' @param engine syberia_engine. The syberia engine to test.
#'    If a \code{character}, it will be passed to \code{\link{syberia_engine}} first.
#' @param base character. Any subdirectory to test specifically. By default,
#'    \code{"test"}.
#' @param config character. The relative path to the configuration resource,
#'    by default \code{"config/environments/test"}.
#' @param ignored_tests character. The list of tests to ignore, by default
#'    the local variable \code{ignored_tests} extracted from the configuration
#'    resource specific by the \code{config} parameter.
#' @param optional_tests character. The list of tests to ignore, by default
#'    the local variable \code{optional_tests} extracted from the configuration
#'    resource specific by the \code{config} parameter.
#' @param required logical. Whether or not all tests are required to have resources,
#' @param required logical. Whether or not all tests are required to have resources,
#'    by default \code{TRUE}. If \code{TRUE}, the \code{ignored_tests}
#'    resources will not be required to have an accompanying test.
#' @seealso \code{\link{syberia_engine}}
#' @export
#' @return \code{TRUE} if all tests pass or will error otherwise. Note this
#'    function uses \code{pblapply} from the pbapply package to
#'    represent progress if available.
test_engine <- function(engine = syberia_engine(), base = "test",
                        config = file.path("config", "environments", "test"),
                        ignored_tests = ignored_tests_from_config(engine, base, config),
                        optional_tests = optional_tests_from_config(engine, base, config),
                        required = TRUE) {
  if (is.character(engine)) {
    engine <- syberia_engine(engine)
  }

  if (!is(engine, "syberia_engine")) {
    stop(m("test_engine_type_error"))
  }

  force(ignored_tests)
  tests <- find_tests(engine, base, ignored_tests)

  if (isTRUE(required)) {
    ensure_resources_are_tested(engine, tests, optional_tests)
  }

  test_resources(engine, tests$active)
}

#' Run the tests on a given set of resources.
#'
#' @param engine syberia_engine. The engine to run the tests on.
#' @param tests character. The character vector of resources to test.
test_resources <- function(engine, tests) {
  ensure_no_global_variable_pollution(check_options = TRUE, {
    find_test_hook(project, type = "setup")$run()

    if (requireNamespace("pbapply", quietly = TRUE)) {
      old_pboptions <- options("pboptions")
      on.exit(options(old_pboptions))
      apply_function <- pbapply::pblapply
    } else {
      apply_function <- lapply
    }

    single_setup <- find_test_hook(engine, type = "single_setup")
    single_teardown <- find_test_hook(engine, type = "single_teardown")

    apply_function(tests, test_resource, engine = engine,
                   setup = single_setup, teardown = single_teardown)
  })

  invisible(TRUE)
}

#' Run the tests for a single resource.
#'
#' @param engine syberia_engine. The engine to run the test on.
#' @param resource character. The resource to test.
#' @param setup. A \code{\link[stagerunner::stageRunner]{stageRunner}} to
#'    execute setup hooks for this test.
#' @param teardown. A \code{\link[stagerunner::stageRunner]{stageRunner}} to
#'    execute teardown hooks for this test.
test_resource <- function(engine, resource, setup, teardown) {
  ensure_no_global_variable_pollution(check_options = TRUE, {
    if (!missing(setup)) {                                      
      setup$.context$resource <- resource
      setup$run()
    }

    suppressMessages(project$resource(resource, recompile = TRUE, recompile. = TRUE))

    if (!missing(teardown)) {
      teardown$.context$resource <- resource
      teardown$run()
    }
  }, desc = paste("running", resource))
}

#' Fetch the test setup or teardown hook, if any exists.
#'
#' The resource \code{config/environments/test} should contain a local variable
#' \code{setup} or \code{teardown} that has a function or list of functions to
#' be incorporated into a stageRunner that will run the actual test setup
#' or teardown.
#'
#' The seed environment for the stageRunner will contain the director object
#' of the relevant project in the key \code{director}.
#'
#' @param project director or character. The director for the syberia project.
#' @param type character. Must be \code{'setup'} or \code{'teardown'}, the former
#'   being the default.
#' @seealso \code{\link{test_project}}
#' @return a stageRunner that will run the relevant setup or teardown hook(s).
find_test_hook <- function(engine, type = 'setup') {
  if (!is.director(engine)) {
    stop("To fetch the ", type, " hook for a project, please pass in a director ",
         "object (the director for the syberia project). Instead I got ",
         "an object of class ", class(engine)[1])
  }

  test_environment_path <- 'config/environments/test'
  if (engine$exists(test_environment_path)) {
    # TODO: (RK) Fix director absolute file paths in $.filename and this hack
    filename <- director:::strip_root(engine$root(),
                                      engine$filename(test_environment_path))
    hooks <- test_environment_config(engine)[[type]] %||% list(force)

    # TODO: (RK) Maybe replace this with a new stageRunner method to check 
    # argument validity? In the future, stageRunner could maybe do more!
    colored_filename <- sQuote(crayon::blue(filename))
    if (!is.list(hooks) && !is.function(hooks) && !is.stagerunner(hooks)) {
      stop("Test ", type, " hooks must be a function or a list of functions.\n\nIn ",
           colored_filename, ", ensure that ",
           "you have ", sQuote(crayon::yellow(paste0(type, ' <- some_function'))),
           " as right now it's an object of class ",
           sQuote(crayon::red(class(hooks)[1])), call. = FALSE)
    }
    if (!is.list(hooks)) hooks <- list(hooks)

    all_have_correct_arity <- is.stagerunner(hooks) || all(rapply(hooks, how = 'unlist',
      function(hook) is.function(hook) && length(formals(hook)) > 0))
    if (!all_have_correct_arity) {
      stop("Test ", type, " hooks must all be functions that take at least one ",
           "argument.\n\nThe first argument will be an environment that has one ",
           "key, ", sQuote('director'), ". In ", colored_filename,
           " ensure your ", sQuote(crayon::yellow(type)),
           " local variable meets this constraint.", call. = FALSE)
    }

    # Do not give access to global environment to ensure modularity.
    hook_env <- new.env(parent = parent.env(globalenv()))
    hook_env$director <- engine

    stageRunner(hook_env, hooks)
  } else stageRunner(new.env(), list(force))
}

 
#' Check that all mandatory tested resources have tests.
#'
#' @param engine syberia_engine. The engine to check.
#' @param tests character. The tests to check. Must be a list with keys
#'     \code{"active"} and \code{"ignored"}.
ensure_resources_are_tested <- function(engine, tests, optional) {
  without_builtin_resources <- function(resources) {
    ## We exclude the `config` and `test` directories.
    resources[substring(resources, 1, 7) != "config/" &
              substring(resources, 1, 5) != "test/"]
  }

  without_optional_resources <- function(resources) {
    Filter(function(resource) !any_is_substring_of(resources, optional), resources)
  }

  all_resources <- without_optional_resources(without_builtin_resources(engine$find()))

  # Error if any resources don't have tests.
  necessary_tests <- file.path("test", all_resources)
  missing_tests   <- setdiff(necessary_tests, c(tests$active, tests$ignored))
  if (length(missing_tests) > 0L) {
    stop(call. = FALSE, "Tests are missing for the following resources:\n\n",
         crayon::red(paste(gsub("^test/", "", missing_tests), collapse = "\n")))
  }
}
  

find_tests <- function(engine, base, ignored_tests) {
  all_tests <- engine$find(children. = FALSE, base = gsub("\\/$", "", base)) 
  tests     <- Filter(function(x) !any_is_substring_of(x, ignored_tests), all_tests)
  ignored_test_paths <- setdiff(all_tests, tests)

  list(
    active  = tests,
    ignored = setdiff(all_tests, tests)
  )
}

test_environment_configuration <- memoise::memoise(
  function(engine, path = file.path("config", "environments", "test")) {
    if (!engine$exists(path, children. = FALSE)) {
      list()
    } else {
      engine$resource(path, children. = FALSE)
    }
  }
)

ignored_tests_from_config <- function(engine, base, config) {
  value_from_config(engine, base, config, "ignored_tests")
}

optional_tests_from_config <- function(engine, base, config) {
  value_from_config(engine, base, config, "optional_tests")
}

value_from_config <- function(engine, base, config, value) {
  file.path(base,
    (test_environment_configuration(engine, config)[[value]]) %||% character(0)
  )
}


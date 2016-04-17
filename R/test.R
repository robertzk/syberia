## Like any good development framework, Syberia offers built-in support for
## testing. The primary unit of development in Syberia is the *engine*.
## All Syberia projects are composed of a collection of engines (each
## potentially depending on vanilla R packages).
##
## This function allows one to run all the *tests* associated with the engine.
## By default, the files in the `test` directory of the engine are considered
## tests, and all other files are non-tests.
##
## The convention is straightforward:
## if you have a Syberia resource `a/b/c` off the root of the project, you
## should have an accompanying test in `test/a/b/c`. Note we leave out the
## `.R` extension as the resource may be an *idempotent* or *non-idempotent*
## resource. 
##
## Idempotent resources are `.R` files in a directory with the same name
## as the file without extension. For example, `test/a/b/c/c.R` would be an
## idempotent resource, and helper files like `test/a/b/c/helper.R` would be
## invisible to the Syberia engine: this encourages clean design and separation
## into helper files as your resource becomes more complex.
##
## Here is an example test. You can look at the accompanying
## [helper project](https://github.com/syberia/syberia/tree/23840b25a8d06740761d6200c7ba68d40c7d7745/tests/testthat/projects/test_calculation_pi)
##
## ```r
## # calculations/pi.R
## # Compute pi using a series: http://functions.wolfram.com/Constants/Pi/06/01/01/
## Reduce(`+`, 4 * vapply(seq_len(1000) - 1, function(k) {
##   (-1)^k / (2 * k + 1)
## }, double(1)))
##
## # test/calculations/pi.R
## test_that("the calculation is close to pi", {
##   # Note we have access to the testthat package.
##   expect_less_than(abs(resource() - 3.1415926), 1e-3)
## })
## ```
## 
## We can execute the test using `test_engine(root)` where `root` is the string
## representing the directory the above files are contained in.
##
## Note that Syberia provides the `resource` helper to fetch the current resource
## being tested. You could pass a first argument to fetch another resource,
## but if you leave it empty, the default will always be the resource corresponding
## to the tested resources. Thus, if you are in `test/calculations/pi`, calling
## `resource()` will build you `calculations/pi`.
## 
## It is possible to add test setup and teardown hooks. This means that before
## the test suite runs, you can add additional conditions to ensure your
## project is working as intended. For example, the author has found it useful
## to add checks for `README.md` files in all directories to encourage
## the team to always add documentation (or else your test suite breaks!).
##
## To add a test setup hook, simply place a function or a list of functions
## into the `setup` local variable in the file `config/environments/test.R`
## (note this is controlled by the `config` parameter to `test_engine`.
## Additional hooks are `teardown` (for when the test suite finishes),
## `single_setup` (every time a test runs), and `single_teardown` (every
## time a test finishes). Any functions or lists of functions passed to
## these locals should have only one argument: the received value will be
## an environment containing the key `resource`, a string representing the
## current resource being tested, and the key `project`, pointing to the
## `syberia_engine` object for the project (or engine).
##
## For example, one could include the following configuration file in a syberia
## project.
##
## ```r
## # config/environments/test.R
## setup    <- function(env) { cat("Beginning test suite!\n") }
## teardown <- function(env) { cat("Ending test suite!\n") }
## single_setup    <- function(env) {
##   cat(paste("Testing resource", env$resource, "\n")) }
## single_teardown <- function(env) {
##   cat(paste("Tested resource", env$resource,  "\n")) }
## ```
## 
## Running `test_engine(...)` on this project will cause the first message
## to be printed, then the last two to alternate between each test,
## before finally wrapping up with the second message.
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
#'    by default \code{TRUE}. If \code{TRUE}, the \code{ignored_tests}
#'    resources will not be required to have an accompanying test. It is highly
#'    recommended that all your projects have full test coverage.
#' @param reporter character. The testthat package test reporter to use. The
#'    options are \code{c("check", "list", "summary", "minimal", "multi", "rstudio",
#'    "silent", "stop", "tap", "teamcity")}, with the default being \code{"summary"}.
#' @param error_on_failure logical. Whether or not to raise an error
#'    if there are any failures. By default, \code{TRUE}.
#' @seealso \code{\link{syberia_engine}}
#' @export
#' @return A list of \code{testthat_results} objects giving the details for
#'    the tests executed on each tested resource. If \code{error_on_failure}
#'    is \code{TRUE}, error instead if there are any failures.
test_engine <- function(engine = syberia_engine(), base = "test",
                        config = file.path("config", "environments", "test"),
                        ignored_tests = ignored_tests_from_config(engine, base, config),
                        optional_tests = optional_tests_from_config(engine, base, config),
                        required = TRUE, reporter = c("summary", "check", "list",
                          "minimal", "multi", "rstudio", "silent", "stop", "tap", "teamcity")[1L],
                        error_on_failure = TRUE) {
  if (is.character(engine)) {
    engine <- syberia_engine(engine)
  }

  if (!is(engine, "syberia_engine")) {
    stop(m("test_engine_type_error"))
  }

  force(ignored_tests)
  ## We will try to identify the resources to test by grabbing all
  ## non-test (i.e., not in the `test` directory) resources from the engine
  ## and also excluding those given by `ignored_tests`.
  tests <- find_tests(engine, base, ignored_tests)

  ## If testing is required for the engine (i.e., every resource needs
  ## an accompanying test) fail unless the condition is satisfied.
  if (isTRUE(required)) {
    ensure_resources_are_tested(engine, tests, optional_tests, base)
  }

  results <- test_resources(engine, tests$active, config, reporter = reporter)

  if (isTRUE(error_on_failure)) {
    if (!all(vapply(results, getFromNamespace("all_passed", "testthat"), logical(1)))) {
      stop("Test failures", call. = FALSE)
    }
  }

  invisible(results)
}

#' Run the tests on a given set of resources.
#'
#' @param engine syberia_engine. The engine to run the tests on.
#' @param tests character. The character vector of resources to test.
#' @param ... Additional arguments to pass to \code{find_test_hook}.
#' @param reporter character. The test reporter to use.
#' @return The testthat result summary for this one test run.
test_resources <- function(engine, tests, ..., reporter) {
  ## We ensure testthat and testthatsomemore are installed.
  ensure_test_packages()

  ## The testthat package does not export `find_reporter`, so we
  ## grab it [with a trick](https://stat.ethz.ch/R-manual/R-devel/library/base/html/ns-reflect.html).
  reporter <- getNamespace("testthat")$find_reporter(reporter)
  ## We are mimicking [testthat's `test_files`](https://github.com/hadley/testthat/blob/6cdd17cab674175297e16e12ac5ed29266534390/R/test-files.r#L43).
  reporter$start_reporter()

  results <- NULL
  ## We would like to error if any global options have been modified,
  ## or global variables introduced.
  ## 
  ## Syberia resources should be stateless and not modify global options!
  ensure_no_global_variable_pollution(check_options = TRUE, {
    setup_hook <- find_test_hook(engine, type = "setup", ...)
    if (!is.null(setup_hook)) setup_hook$run()

    single_setup    <- find_test_hook(engine, type = "single_setup", ...)
    single_teardown <- find_test_hook(engine, type = "single_teardown", ...)
    results <- lapply(tests, test_resource, engine = engine, reporter = reporter,
                      setup = single_setup, teardown = single_teardown)
  })

  reporter$end_reporter()
  ## Again mimicking [testthat](https://github.com/hadley/testthat/blob/6cdd17cab674175297e16e12ac5ed29266534390/R/test-files.r#L50).
  invisible(getNamespace("testthat")$testthat_results(results))
}

ensure_test_packages <- function() {
  lapply(c("testthat", "testthatsomemore"), ensure_installed)
}

#' Run the tests for a single resource.
#'
#' @param engine syberia_engine. The engine to run the test on.
#' @param resource character. The resource to test.
#' @param setup stageRunner. A \code{\link[stagerunner]{stageRunner}} to
#'    execute setup hooks for this test.
#' @param teardown stageRunner. A \code{\link[stagerunner]{stageRunner}} to
#'    execute teardown hooks for this test.
#' @param reporter reporter. A testthat reporter object. 
#' @return The testthat result summary for this one test run.
test_resource <- function(engine, resource, setup, teardown, reporter) {
  result <- NULL

  ## Each resource itself should not add global variables or pollute options.
  ensure_no_global_variable_pollution(check_options = TRUE, {
    if (!missing(setup) && !is.null(setup)) {
      setup$.context$resource <- resource
      setup$run()
    }

    call_args <- list(resource, recompile = TRUE, recompile. = TRUE)
    if (!missing(reporter)) {
      call_args$reporter <- reporter
    }
    result <- suppressMessages(do.call(engine$resource, call_args))

    if (!missing(teardown) && !is.null(teardown)) {
      teardown$.context$resource <- resource
      teardown$run()
    }
  ## The `desc` parameters allows us to be specific when we fail:
  ## it will inform the user the name of the resource that created a global
  ## variable or modified a global option.
  }, desc = paste("running", resource))

  result
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
#' @param engine syberia_engine. The director for the syberia project.
#' @param type character. Must be \code{'setup'} or \code{'teardown'}, the former
#'   being the default.
#' @param config character. The resource used to fetch configuration.
#' @seealso \code{\link{test_engine}}
#' @return a stageRunner that will run the relevant setup or teardown hook(s).
find_test_hook <- function(engine, type = "setup", config) {
  if (!is(engine, "syberia_engine")) {
    stop(m("test_hook_not_engine", type = type, klass = class(engine)[1L]), call. = FALSE)
  }

  hooks <- value_from_config(engine, config, type)
  if (is.null(hooks)) return(NULL)

  # TODO: (RK) Maybe replace this with a new stageRunner method to check 
  # argument validity? In the future, stageRunner could maybe do more!
  colored_filename <- sQuote(crayon::blue(config))
  if (!is.list(hooks) && !is.function(hooks) && !stagerunner::is.stagerunner(hooks)) {
    stop(m("test_hook_invalid_format", type = type, filename = colored_filename,
           klass = class(hooks)[1L]), call. = FALSE)
  }

  if (!is.list(hooks)) { hooks <- list(hooks) }

  all_have_correct_arity <- stagerunner::is.stagerunner(hooks) ||
    all(rapply(hooks, how = "unlist", function(hook) {
      is.function(hook) && length(formals(hook)) > 0
    }))

  if (!all_have_correct_arity) {
    stop(m("test_hook_arity_error", type = type, filename = colored_filename,
           type = type), call. = FALSE)
  }

  # Do not give access to global environment to ensure modularity.
  hook_env <- list2env(list(project = engine), parent = parent.env(globalenv()))
  stagerunner::stageRunner(hook_env, hooks)
}

 
#' Check that all mandatory tested resources have tests.
#'
#' @param engine syberia_engine. The engine to check.
#' @param tests character. The tests to check. Must be a list with keys
#'     \code{"active"} and \code{"ignored"}.
#' @param optional character. A character vector of optional tests.
#' @param base character. The directory containing tests in the project, by
#'     default \code{"test"}.
#' @return Nothing, but error if not all resources have tests.
ensure_resources_are_tested <- function(engine, tests, optional, base = "test") {
  without_builtin_resources <- function(resources) {
    ## We exclude the `config` and `test` directories.
    resources[substring(resources, 1, 7) != "config/" &
              substring(resources, 1, 5) != paste0(base, "/")]
  }

  without_optional_resources <- function(resources) {
    ## `optional` resources are actually substring matches!
    Filter(function(resource) !any_is_substring_of(resource, optional), resources)
  }

  all_resources <- without_optional_resources(without_builtin_resources(engine$find()))

  # Error if any resources don't have tests.
  necessary_tests <- file.path(base, all_resources)
  missing_tests   <- setdiff(necessary_tests, c(tests$active, tests$ignored))
  if (length(missing_tests) > 0L) {
    stop(call. = FALSE, "Tests are missing for the following resources:\n\n",
         crayon::red(paste(gsub(paste0("^", base, "/"), "", missing_tests), collapse = "\n")))
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

## By default, we will fetch the test configuration from
## `config/environments/test`, but the location of the configuration file
## itself is configurable (see the `config` parameter to `test_engine`).
test_environment_configuration <- 
  function(engine, path = file.path("config", "environments", "test")) {
    if (!engine$exists(path, children. = FALSE)) {
      list()
    } else {
      engine$resource(path, children. = FALSE)
    }
  }

## If the configuration file, usually `config/environments/test.R`,
## has a local variable `ignored_tests`, the tests of the 
## character vector of resource names will not be executed.
## This is useful when certain tests are broken or under development.
ignored_tests_from_config <- function(engine, base, config) {
  file.path(base, 
    value_from_config(engine, config, "ignored_tests") %||% character(0)
  )
}

## If the configuration file, usually `config/environments/test.R`,
## has a local variable `optional_tests`, the character vector of resource
## names will not be executed. This is useful when certain resources
## were never expected to have tests, such as those representing network
## connections, constants, or other things that don't really make sense to test.
##
## Use with caution!
optional_tests_from_config <- function(engine, base, config) {
  value_from_config(engine, config, "optional_tests") %||% character(0)
}

value_from_config <- function(engine, config, value) {
  test_environment_configuration(engine, config)[[value]]
}


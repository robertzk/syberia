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
                        required = TRUE) {
  if (is.character(engine)) {
    engine <- syberia_engine(engine)
  }

  if (!is(engine, "syberia_engine")) {
    stop(m("test_engine_type_error"))
  }

  force(ignored_tests)
  tests <- find_tests(engine, base, ignored_tests)

  ensure_resources_have_tests

  # TODO: (RK) Actually run the tests.
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

test_environment_configuration <- function(engine, path = file.path("config", "environments", "test")) {
  if (!engine$exists(path, children. = FALSE)) {
    list()
  } else {
    engine$resource(path, children. = FALSE)
  }
}

ignored_tests_from_config <- function(engine, base, config) {
  file.path(base,
    (test_environment_configuration(engine, config)$ignored_tests) %||% character(0)
  )
}


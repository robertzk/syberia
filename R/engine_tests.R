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
#' @seealso \code{\link{syberia_engine}}
#' @export
#' @return \code{TRUE} if all tests pass or will error otherwise. Note this
#'    function uses \code{pblapply} from the pbapply package to
#'    represent progress if available.
test_engine <- function(engine = syberia_engine(), base = "test") {
  if (is.character(engine)) {
    engine <- syberia_engine(engine)
  }

  if (!is(engine, "syberia_engine")) {
    stop(m("test_engine_type_error"))
  }
}


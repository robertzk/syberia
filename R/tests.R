#' Run all tests in a syberia project.
#'
#' The tests that will be run are all those in the \code{test} subdirectory
#' of the root of the syberia project.
#'
#' By default, no test setup or teardown occurs. That is, there is no code that
#' is executed before all tests run and after all tests run. However, there do
#' exist hooks to provide this behavior. This can also be used to perform
#' additional testing not covered by sourcing all files in the "test/" directory
#' of the syberia project.
#'
#' To provide a setup or teardown hook, simply place a function or list of
#' functions in a local variable \code{setup} or \code{teardown}, respectively,
#' in \code{config/environments/test} relative to the root of the syberia project.
#'
#' For example, creating a file \code{config/environments/test.R} with the code
#' code \code{setup <- function(env) cat("Running all tests.")} will print a message
#' before all the tests are run. The one parameter the function must take is an
#' environment which will contain a single key, `director`, pointing to the 
#' `director` object coming from `syberia_project`.
#'
#' @param project director or character. The director for the syberia project.
#'    If a \code{character}, it will be passed to \code{syberia_project} first.
#' @seealso \code{\link{syberia_project}}
#' @export
#' @return \code{TRUE} if all tests pass or will error otherwise. Note this
#'    function uses \code{pblapply} from the \code{pbapply} package to
#'    represent progress.
test_project <- function(project, base = '') {
  if (is.character(project)) project <- syberia_project(project)
  test_path <- file.path(project$root(), 'test')
  tests <- project$find(base = paste0('test/', base))

  # TODO: (RK) Check READMEs
  # TODO: (RK) Check all resources have tests, except those w/ test = FALSE in routes
  # TODO: (RK) In config/environments/test, allow test hooks for additional testing.
  
  # Perform test setup  

  # Run all tests
  Ramd::packages('pbapply')
  pblapply(tests, function(t) suppressMessages(project$resource(t)$value()))

  invisible(TRUE)
}

#' Fetch the test setup hook, if any exists.
#'
#' The resource \code{config/environments/test} should contain a local variable
#' \code{setup} that has a function or list of functions to be incorporated
#' into a stageRunner that will run the actual test setup.
#'
#' The seed environment for the stageRunner will contain the director object
#' of the relevant project in the key \code{director}.
#'
#' @param project director or character. The director for the syberia project.
#' @seealso \code{\link{test_projet}}
#' @return a stageRunner that will run the relevant setup hook(s).
test_setup <- function(project) {
  if (!is.director(project)) {
    stop("To fetch the setup hook for a project, please pass in a director ",
         "object (the director for the syberia project). Instead I got ",
         "an object of class ", class(project)[1])
  }

  if (project$exists('config/environments/test')) {
    # Do not give access to global environment to ensure modularity.
    setup_hook_env <- new.env(parent = parent.env(globalenv()))

    #stageRunner(setup_hook_env, 
  } else stageRunner(new.env(), list())
}


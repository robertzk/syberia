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
  
  ensure_no_global_variable_pollution({
    test_hook(project, type = 'setup')$run() # Run the test setup hook stageRunner

    # Run all tests
    Ramd::packages('pbapply')
    pblapply(tests, function(t) suppressMessages(project$resource(t)$value()))

    # TODO: (RK) Populate teardown stageRunner environment with test info?
    # Could be useful to some people.
    test_hook(project, type = 'teardown')$run() # Run the test teardown hook stageRunner
  }, desc = "running this project's tests (this is bad and should never happen)")

  invisible(TRUE)
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
test_hook <- function(project, type = 'setup') {
  if (!is.director(project)) {
    stop("To fetch the ", type, " hook for a project, please pass in a director ",
         "object (the director for the syberia project). Instead I got ",
         "an object of class ", class(project)[1])
  }

  test_environment_path <- 'config/environments/test'
  if (project$exists(test_environment_path)) {
    # TODO: (RK) Fix director absolute file paths in $.filename and this hack
    filename <- director:::strip_root(project$root(),
                                      project$.filename(test_environment_path))
    test_environment_config <- project$resource(test_environment_path)$value()
    hooks <- test_environment_config[[type]] %||% list()

    # TODO: (RK) Maybe replace this with a new stageRunner method to check 
    # argument validity? In the future, stageRunner could maybe do more!
    colored_filename <- sQuote(director:::colourise(filename, 'blue'))
    if (!is.list(hooks) && !is.function(hooks) && !is.stagerunner(hooks)) {
      stop("Test ", type, " hooks must be a function or a list of functions.\n\nIn ",
           colored_filename, ", ensure that ",
           "you have ", sQuote(director:::colourise(paste0(type, ' <- some_function'), 'yellow')),
           " as right now it's an object of class ",
           sQuote(director:::colourise(class(hooks)[1], 'red')), call. = FALSE)
    }
    if (!is.list(hooks)) hooks <- list(hooks)

    have_correct_arity <- rapply(hooks, how = 'unlist',
      function(hook) is.function(hook) && length(formals(hook)) > 0)
    if (!all(have_correct_arity)) {
      stop("Test ", type, " hooks must all be functions that take at least one ",
           "argument.\n\nThe first argument will be an environment that has one ",
           "key, ", sQuote('director'), ". In ", colored_filename,
           " ensure your ", sQuote(director:::colourise(type, 'yellow')),
           " local variable meets this constraint.", call. = FALSE)
    }

    # Do not give access to global environment to ensure modularity.
    hook_env <- new.env(parent = parent.env(globalenv()))
    hook_env$director <- project

    stageRunner(hook_env, hooks)
  } else stageRunner(new.env(), list())
}


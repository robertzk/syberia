# A list of convenient helper functions for interacting with syberia
# projects through the console.

#' Auto run a stage in the last syberia active runner.
#'
#' This is used in conjunction with \code{base::addTaskCallback} when running
#' a model from the interactive console.
#'
#' @param x expression. The last executed R expression. If of the form
#'   \code{a / b}, it will translate to \code{active_runner()$run(, "a/b")}.
#' @param ... additional arguments the task callback usually receives.
#' @seealso \code{\link{addTaskCallback}}
#' @examples 
#' # From console, type:
#' # > 2/5
#' # and the fifth substage of the second stage of the last active runner
#' # will be executed.
auto_run <- function(x, ...) {
  if (is.call(x) && length(x) >= 1 && x[[1]] == '/' &&
      (is.numeric(x[[2]]) || is.numeric(x[[3]])) &&
      is.stagerunner(active_runner())) {
    run_commands <- deparse(x)
    run_commands <- strsplit(run_commands, ":")[[1]]
    tryCatch(finally = return(TRUE), { if (length(run_commands) == 2)
      run_model(, run_commands[[1]], run_commands[[2]])
    else if (length(run_commands) == 1)
      run_model(, run_commands[[1]])
    })
  }
  TRUE
}


#' Run all tests in a syberia project.
#'
#' TODO: (RK) Define custom tests for invisible resources.
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
  # Run all tests
  Ramd::packages('pbapply')
  pblapply(tests, function(t) suppressMessages(project$resource(t)$value()))
  invisible(TRUE)
}


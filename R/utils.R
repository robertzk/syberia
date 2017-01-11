#' Non-null selection operator.
#'
#' @name infix_or
#' @param x ANY. An R object. If \code{NULL}, return \code{y}.
#' @param y ANY. An R object. If x is \code{NULL}, return \code{y}.
#' @export
`%||%`   <- function(x, y) if (is.null(x)) y else x

`%|||%`  <- function(x, y) if (is.falsy(x)) y else x

is.falsy <- function(x) {
  identical(x, NULL) || identical(x, FALSE) || identical(x, "") ||
  length(x) == 0 || identical(x, 0)
}

#' Retrieve Github personal access token.
#'
#' Borrowed from \url{https://github.com/r-pkgs/remotes/blob/master/R/github.R#L23}
#' A github personal access token
#' Looks in env var \code{GITHUB_PAT}
#'
#' @keywords internal
#' @noRd
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (!nzchar(pat)) { return(NULL) }

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}

#' Fetch the current Syberia version.
#' @export
#' @return The version of the syberia package as \code{\link{package_version}}
#'   object.
syberia_version <- function() {
  utils::packageVersion("syberia")
}

package_exists <- function(name) {
  is.element(name, utils::installed.packages()[, 1])
}

ensure_installed <- function(package_name) {
  ## Using [`requireNamespace`](http://r-pkgs.had.co.nz/src.html)
  ## is the de facto accepted approach here.
  if (!package_exists(package_name)) {
    stop("Please install ", crayon::yellow(package_name), ":\n\n",
         crayon::green(paste0("install.packages('", package_name, "')")), "\n", call. = FALSE)
  }
}

## [Testthatsomemore](https://github.com/robertzk/testthatsomemore)
## is an auxiliary package used for some testing utilities.
ensure_testthatsomemore <- function() {
  if (package_exists("testthatsomemore")) return()
  ensure_installed("devtools")
  message("The package ", crayon::yellow("testthatsomemore"),
          " is not installed; installing from http://github.com/robertzk/testthatsomemore")
  withCallingHandlers({
    ## We install it from GitHub if the user does not have it installed.
    devtools::install_github("robertzk/testthatsomemore")
    requireNamespace("testthatsomemore", quietly = TRUE)
  }, error = function(e) {
    stop("The ", crayon::red("testthatsomemore"), " package failed to install. ",
         "Try manually: \n\n",
         crayon::green('devtools::install_github("robertzk/testthatsomemore")'), "\n\n",
         "The error was: ", paste(as.character(e), collapse = "\n"), call. = FALSE)
  })
}

as.list.environment <- function(env) {
  out <- base::as.list.environment(env)
  lapply(out, function(x) if (is.environment(x) && !is(x, "R6")) as.list(x) else x)
}

#' Ensure no global variables are polluted during an expression.
#'
#' If any global variables are removed or created, it will
#' give a descriptive error.
#'
#' @param expr expression. The R expression to evaluate
#' @param desc character. A string to add to "you modified global
#' @param check_options logical. Whether to check if any global options were changed.
#'   variables while [\code{desc} goes here]".
#' @return the output of the \code{expr}.
ensure_no_global_variable_pollution <- function(expr, desc, check_options = FALSE) {
  if (isTRUE(check_options)) old_options <- options()
  before <- ls(globalenv())

  out <- eval.parent(substitute(expr))

  after <- ls(globalenv())
  missing_desc <- missing(desc)
  shorten <- function(vars) if (length(vars) > 5) c(vars[1:5], "...") else vars
  message <- function(vars, type = "variables", action = "removed") {
    msg <- paste("Some global", type, "were", action)
    if (!eval.parent(quote(missing_desc))) msg <- paste(msg, "while", desc)
    msg <- paste0(msg, ": ", crayon::red(paste(vars, collapse = ", ")))
  }

  check_before_after <- function(before, after, type) {
    if (length(bads <- setdiff(before, after)) > 0) stop(message(bads, type = type))
    else if (length(bads <- setdiff(after, before)) > 0)
      stop(message(bads, type = type, action = "added"))
  }

  check_before_after(before, after, "variables")

  if (isTRUE(check_options) && !identical(new_options <- options(), old_options)) {
    before <- ls(old_options); after <- ls(new_options)
    check_before_after(before, after, "options")
    diffs <- vapply(before,
      function(name)! identical(old_options[[name]], new_options[[name]]), logical(1))
    stop("Some global options were modified: ",
         crayon::red(paste(names(which(diffs)), collapse = ", ")))
  }

  out
}

#' Perform an action repeatedly on parent directories until success or error.
#'
#' Given a \code{fn}, we may wish to run it on a \code{filepath}, determine
#' its success, and try again with the parent directory of \code{filepath},
#' until we obtain result that is not \code{NULL}. If this does not occur for
#' any parent directory, we halt with the string \code{error}.
#'
#' @param filepath character. The filepath to traverse along. The \code{fn}
#'    function will be called with \code{filepath} and its parent directories
#'    until it returns a result other than \code{NULL}.
#' @param fn function. A one-argument function called on \code{filepath} or
#'    its successive parent directories until a result other than \code{NULL}
#'    is returned, which will be the final return value.
#' @param error character or function. A string to error if \code{fn} returns
#'    \code{NULL} on all parent directories, or a one-argument function to
#'    execute (the argument received will be the initial \code{filepath})
#' @return The result of \code{fn} on the first parent directory of
#'   \code{filepath} on which it is not \code{NULL}.
traverse_parent_directories <- function(filepath, fn, error) {
  stopifnot(is.character(filepath), length(filepath) == 1, !is.na(filepath))
  stopifnot(is.function(fn), length(formals(fn)) >= 1)
  stopifnot(is.character(error) || is.function(error))

  path <- normalizePath(filepath, mustWork = FALSE)

  ## As long as we have not hit the root directory, keep trying.
  while (!identical(dirname(path), path)) {
    result <- fn(path)
    if (!is.null(result)) return(result)
    path <- dirname(path)
  }

  if (is.character(error)) stop(error)
  else error(filepath)
}

order_by_key <- function(list) {
  list[order(names(list))]
}

is.simple_string <- function(obj) {
  is.character(obj) && length(obj) == 1 && !is.na(obj) && nzchar(obj)
}

#' Whether or not any substring of a string is any of a set of strings.
#'
#' @param string character.
#' @param set_of_strings character.
#' @return logical
#' @examples
#' stopifnot(syberia:::any_is_substring_of('test', c('blah', 'te', 'woo'))) # TRUE
#' stopifnot(!syberia:::any_is_substring_of('test', c('blah', 'woo'))) # FALSE
any_is_substring_of <- function(string, set_of_strings) {
  any(vapply(set_of_strings,
             function(x) substring(string, 1, nchar(x)) == x, logical(1)))
}

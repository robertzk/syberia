`%||%`   <- function(x, y) if (is.null(x)) y else x
`%|||%`  <- function(x, y) if (is.falsy(x)) y else x
is.falsy <- function(x) {
  identical(x, NULL) || identical(x, FALSE) || identical(x, "") ||
  length(x) == 0 || identical(x, 0)
}

#' Fetch the current Syberia version.
#' @export
syberia_version <- function() { utils::packageVersion('syberia') }

package_exists <- function(name) {
  is.element(name, base::.packages())
}

as.list.environment <- function(env) {
  out <- base::as.list.environment(env)
  lapply(out, function(x) if (is.environment(x)) as.list(x) else x)
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
ensure_no_global_variable_pollution <- function(expr, desc, check_options = FALSE) {
  if (isTRUE(check_options)) old_options <- options()
  before <- ls(globalenv())

  out <- eval.parent(substitute(expr))

  after <- ls(globalenv())
  missing_desc <- missing(desc)
  shorten <- function(vars) if (length(vars) > 5) c(vars[1:5], '...') else vars
  message <- function(vars, type = 'variables', action = 'removed') {
    msg <- paste("Some global", type, "were", action)
    if (!eval.parent(quote(missing_desc))) msg <- paste(msg, "while", desc)
    msg <- paste0(msg, ": ", crayon::red(paste(vars, collapse = ", ")))
  }

  check_before_after <- function(before, after, type) {
    if (length(bads <- setdiff(before, after)) > 0) stop(message(bads, type = type))
    else if (length(bads <- setdiff(after, before)) > 0)
      stop(message(bads, type = type, action = 'added'))
  }

  check_before_after(before, after, 'variables')

  if (isTRUE(check_options) && !identical(new_options <- options(), old_options)) {
    before <- ls(old_options); after <- ls(new_options)
    check_before_after(before, after, 'options')
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

  path <- normalizePath(filepath)
  ## As long as we have not hit the root directory, keep trying
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


#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch the current Syberia version.
#' @export
syberia_version <- function() utils::packageVersion('syberia')

package_exists <- function(name) {
  is.element(name, base::.packages())
}

as.list.environment <- function(env) {
    out <- base::as.list.environment(env)
  lapply(out, function(x) if (is.environment(x)) as.list(x) else x)
}

#' Normalize a file name.
#'
#' Given a string, will check if a file by that name exists.
#' If not, it will try to append '.r' and try again.
#' If not, it will try to append '.R' and try again.
#' Otherwise, it returns FALSE.
#'
#' @param filename a character. The name of a file.
#' @examples
#' \dontrun{
#' normalize_filename('test') # will find test.r or test.R
#' normalize_filename('test.r') # just returns test.r
#' }
normalized_filename <- function(filename) {
  # TODO: Extend to regular expressions
  if (file.exists(filename)) filename
  else if (file.exists(tmp <- pp("#{filename}.r"))) tmp
  else if (file.exists(tmp <- pp("#{filename}.R"))) tmp
  else FALSE
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


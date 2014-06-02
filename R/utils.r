#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch the current Syberia version.
#' @export
syberia_version <- function() utils::packageVersion('syberia')

package_exists <- function(name) {
  name %in% rownames(utils::installed.packages())
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

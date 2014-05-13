`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch the current Syberia version.
#' @export
syberia_version <- function() utils::packageVersion('syberia')

library(testthatsomemore)

not_found <- function() { structure(list(), class = 'not_found') }
is.not_found <- function(x) { is(x, 'not_found') }
with_globals <- function(list, expr) {
  pre_existing <- mget(names(list), envir = globalenv(),
                       ifnotfound = replicate(length(list), not_found()))
  pre_existing <- Filter(Negate(is.not_found), pre_existing)
  on.exit({
    for (name in names(list)) {
      if (name %in% names(pre_existing)) {
        base::assign(name, pre_existing[[name]], envir = globalenv())
      } else {
        base::rm(name, envir = globalenv())
      }
    }
  }, add = TRUE)
  for (name in names(list)) {
    assign(name, list[[name]], envir = globalenv())
  }
  eval.parent(substitute(expr))
}

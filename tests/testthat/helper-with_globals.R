with_globals <- function(list, expr) {
  pre_existing <- mget(names(list), envir = globalenv(), inherits = FALSE)
  on.exit(add = TRUE, {
    for (name in names(list)) {
      if (name %in% names(pre_existing)) {
        base::assign(name, pre_existing[[name]], envir = globalenv())
      } else {
        base::rm(name, envir = globalenv())
      }
    }
  })
  for (name in names(list)) {
    assign(name, list[[name]], envir = globalenv())
  }
  eval.parent(substitute(expr))
}

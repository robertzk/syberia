with_test_mock <- function(expr) {
  env <- parent.frame()
  repeat {
    env <- parent.env(env)
    if (identical(env, emptyenv())) { stop("Cannot mock this path.") }
    if (identical(attr(env, "name"), "imports:syberia")) { break }
  }
  # TODO: (RK) Refactor this to stub arbitrary methods.
  unlockBinding("expect_equal", env)
  unlockBinding("test_that", env)
  old_methods <- mget(c("expect_equal", "test_that"), envir = env)
  on.exit(list2env(old_methods, envir = env), add = TRUE)
  on.exit(lockBinding("expect_equal", env), add = TRUE)
  on.exit(lockBinding("test_that", env), add = TRUE)
  assign("expect_error_", env$expect_error, envir = env)
  assign("expect_equal", function(...) stop("Didn't match"), envir = env)
  assign("test_that", function(msg, expr) eval.parent(substitute(expr)), envir = env)
  eval.parent(substitute(expr))
}

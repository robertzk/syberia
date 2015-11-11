# Any variables defined in this file will become global variables
# as soon as this project is loaded. They are meant as shortcuts / mnemonics.

mungebits <- function(...) director$find(..., method = 'substring', '^lib/mungebits')
classifiers <- function(...) director$find(..., method = 'substring', '^lib/classifiers')
adapters <- function(...) director$find(..., method = 'substring', '^lib/adapters')
controllers <- function(...) director$find(..., method = 'substring', '^lib/controllers')
shared <- function(...) director$find(..., method = 'substring', '^lib/shared')
stages <- function(...) director$find(..., method = 'substring', '^lib/stages')
run <- function(path, ...) runner(path)$run(..., verbose = TRUE)

runner <- function(version) {
  res <- Filter(function(x) grepl('^/?models/', x), director$find(version))[1]
  if (is.na(res)) stop("No model version ", sQuote(version), call. = FALSE)
  resource(res)
}

stest <- function(filter) {
  library(testthat)
  if (missing(filter)) test_project(root()) # Run all tests in this repo
  else {
    tests <- director$find(filter, base = 'test')
    for (test in tests) director$resource(test)$value(recompile = TRUE, recompile. = TRUE) # Run test
  }
}

reload_syberia <- function(...) {
  unloadNamespace('syberia')
  unloadNamespace('syberiaStages')
  library(syberia)
  syberia_project(root())
  invisible(TRUE)
}

last_model    <- function() { director$cache$get('last_model') }
last_run      <- function() { director$cache$get('last_run') }
active_runner <- function() { director$cache$get('last_model_runner') }

keymap <- list(
  A = function() active_runner()$context$data,
  M = function() A$model_stage$model,
  S = function() active_runner()
)
suppressWarnings(lapply(names(keymap), function(key) {
  eval(parse(text = paste0("rm(", key, ")")), env = globalenv())
  makeActiveBinding(key, keymap[[key]], env = globalenv())
}))

`%||%` <- function(x, y) if (is.null(x)) y else x

.onAttach <- function(...) {
  if (!isTRUE(getOption("syberia.silent"))) {
    packageStartupMessage(paste0("Loading ", crayon::red("Syberia"), "...\n"))
  }

  # Load better trace.
  if (isTRUE(getOption("syberia.autoload_bettertrace", TRUE)) &&
      !identical(Sys.getenv("CI"), "TRUE")) {
    if (!is.element("devtools", utils::installed.packages()[, 1])) {
      packageStartupMessage(crayon::yellow("   ...Installing devtools\n"))
      utils::install.packages("devtools")
    }

    if (!requireNamespace("bettertrace", quietly = TRUE)) {
      packageStartupMessage(crayon::yellow("   ...Installing github.com/robertzk/bettertrace\n"))
      devtools::install_github("robertzk/bettertrace")
    }
    library(bettertrace)
  }

  makeActiveBinding("project", env = globalenv(),
    function() getFromNamespace("active_project", "syberia")())
  makeActiveBinding("resource", env = globalenv(),
    function(...) getFromNamespace("active_project", "syberia")()$resource(...))
  
  # We want to initialize a Syberia project in the current working directory
  # because 9 times out of 10 this is what the user wants.
  #
  # However, this hook doesn't work on install, because install is done from
  # the working directory of the package, not the user.
  #
  # But when the user calls library(syberia), this will work.
  try({
    syberia_engine()
    packageStartupMessage(crayon::yellow(
      "Loaded syberia project ", sQuote(active_project()$root()), "...\n"
    ))
  }, silent = TRUE)
}

.onDetach <- function(...) {
  try(silent = TRUE, detach("syberia:shims"))
  # TODO: (RK) Detach any environments attached to the search path
  # by the active project.
}

.onUnload <- function(...) {
  try(silent = TRUE, detach("syberia:shims"))
}


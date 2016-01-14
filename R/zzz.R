install_options <- function() {
  # Use Rstudio as the default CRAN mirror.
  c(repos = getOption("repos", structure(c(CRAN = "http://cran.rstudio.com"))))
}

install_bettertrace <- function() {
  devtools::with_options(install_options(), {
    if (!is.element("devtools", utils::installed.packages()[, 1])) {
      packageStartupMessage(crayon::yellow("   ...Installing devtools\n"))
      utils::install.packages("devtools")
    }

    if (!requireNamespace("bettertrace", quietly = TRUE)) {
      packageStartupMessage(crayon::yellow("   ...Installing github.com/robertzk/bettertrace\n"))
      devtools::install_github("robertzk/bettertrace")
    }
    library(bettertrace)
  })
}

.onAttach <- function(...) {
  if (!isTRUE(getOption("syberia.silent"))) {
    packageStartupMessage(paste0("Loading ", crayon::red("Syberia"), "...\n"))
  }

  # Load bettertrace.
  if (isTRUE(getOption("syberia.autoload_bettertrace", TRUE)) &&
      !identical(Sys.getenv("CI"), "TRUE")) {
    install_bettertrace()
  }

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
  # TODO: (RK) Detach any environments attached to the search path
  # by the active project.
}

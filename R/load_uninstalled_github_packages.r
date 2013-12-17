load_uninstalled_github_packages <- function() {
  package_names <-
    vapply(.github_packages, function(pkg) pkg[[1]], character(1))
  uninstalled_packages <-
    .github_packages[sapply(package_names, Negate(package_exists))]
  lapply(uninstalled_packages, function(pkg) {
    pkg <- as.list(pkg)
    do.call("install_github", pkg,
            envir = as.environment('package:devtools'))
    if (!suppressWarnings(require(pkg[[1]], character.only = TRUE)))
      stop("Failed to load package ", pkg[[1]], " from Github.")
  })
}

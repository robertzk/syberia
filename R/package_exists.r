package_exists <- function(name) {
  name %in% rownames(installed.packages())
}

package_exists <- function(name) {
  name %in% rownames(utils::installed.packages())
}

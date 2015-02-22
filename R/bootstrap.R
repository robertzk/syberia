#' Bootstrap a new syberia project
#'
#' @param name character. The name of the folder where the project should be created.
#' @export
syberia_new <- function(name) {
  stopifnot(is.character(name) && length(name) <= 1)
  npm <- system2("which", "npm")[[1]]
  # Install necessary software
  if (npm == 1) stop("Please install npm first. https://www.npmjs.com")
  yo <- system2("npm", "list -g yo --depth=0")[[1]]
  if (yo == 1) system2("npm", "install -g yo")
  yo_syberia <- system2("npm", "list -g generator-syberia --depth=0")[[1]]
  if (yo_syberia == 1) system2("npm", "install -g generator-syberia")

  wd <- getwd()
  if (!missing(name)) {
    system(paste("mkdir -p", name))
    setwd(paste(wd, name, sep="/"))
  }
  system("yo syberia")
}

#' Bootstrap a new model for a syberia project
#'
#' @param name character. The name of the model.
#' @export
syberia_new_model <- function(name) {
  stopifnot(is.character(name) && length(name) == 1)
  if (is.null(syberia_root())) stop("You need to create a syberia project first. Check if you are in the right directory.")
  system(paste("yo syberia:model", name))
}

#' Bootstrap a new classifier for a syberia project
#'
#' @param name character. The name of the classifierb
#' @export
syberia_new_classifier <- function(name) {
  stopifnot(is.character(name) && length(name) == 1)
  if (is.null(syberia_root())) stop("You need to create a syberia project first. Check if you are in the right directory.")
  system(paste("yo syberia:classifier", name))
}

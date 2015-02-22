#' Bootstrap a new syberia project
#'
#' @param name character. The name of the folder where the project should be created.
#' @export
syberia_new <- function(name) {
  stopifnot(is.character(name) && length(name) <= 1)
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
  system(paste("yo syberia:model", name))
}

#' Bootstrap a new classifier for a syberia project
#'
#' @param name character. The name of the classifier
#' @export
syberia_new_classifier <- function(name) {
  stopifnot(is.character(name) && length(name) == 1)
  system(paste("yo syberia:classifier", name))
}

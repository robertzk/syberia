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

available_types <- c("model", "classifier", "mungebit")

syberia_subgenerator <- function(name, type) {
  stopifnot(is.character(type) && length(type) == 1)
  if (!type %in% available_types)
    stop("Syberia can currently generate only thos types:", model, sep=" ")
  if (is.null(syberia_root())) stop("You need to create a syberia project first. Check if you are in the right directory.")
  name <- ifelse(missing(name), "", name)
  system(paste0("yo syberia:", type, " ", name))
}

#' Bootstrap a new model for a syberia project
#'
#' @param name character. The name of the model.
#' @export
syberia_new_model <- function(name) syberia_subgenerator(name, type = 'model')


#' Bootstrap a new classifier for a syberia project
#'
#' @param name character. The name of the classifier.
#' @export
syberia_new_classifier <- function(name) syberia_subgenerator(name, type = 'classifier')

#' Bootstrap a new mungebit for a syberia project
#'
#' @param name character. The name of the mungebit.
#' @export
syberia_new_mungebit <- function(name) syberia_subgenerator(name, type = 'mungebit')

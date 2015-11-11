#' Create a new Syberia project.
#'
#' @param project_name character. The name of the project to create.
#' @param change_dir logical. If \code{TRUE}, will change the R working directory
#'   to the new Syberia project.  Defaults \code{TRUE}.
#' @param load_project logical. If \code{TRUE}, will source the .Rprofile for the
#'   project and initialize the project.  Defaults \code{TRUE}.
syberia_create <- function(project_name, change_dir = TRUE, load_project = TRUE) {
  dir.create(project_name)
  project_dir <- file.path(getwd(), project_name)
  system(paste0("cp -R ", path.package("syberia"), "/inst/syberia/ ", project_dir))
  message(crayon::green("New Syberia project created..."))
  if (isTRUE(change_dir)) {
    setwd(project_dir)
    if (isTRUE(load_project)) {
      source(".Rprofile")
      syberia_project(project_dir)
    }
  }
}

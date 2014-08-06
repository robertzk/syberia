#' Fetch a syberia project director relative a filename.
#'
#' @param filename character. Some in a syberia project.
#' @return the director object for the syberia project.
#' @export
#' @examples
#' \dontrun{
#'   # Pretend you have the file blah/foo.R in your syberia project ~/proj.
#'   syberia_projects('~/proj/blah/foo.R')
#'   # Now we have the director object for the ~/proj syberia project.
#' }
syberia_project <- local({
  # A cache of directors for the existent syberia projects
  syberia_projects <- list()

  function(filename) {
    if (!(is.character(filename) && length(filename) == 1)) {
      stop("To fetch a syberia project, you must specify a file path ",
           "(a character vector of length 1). Instead we got a ",
           class(filename)[1], if (is.character(filename)) paste0(
           ' of length ', length(filename)))
    }
    # TODO: (RK) Don't go through syberia_root here
    root <- normalizePath(syberia_root(filename))
    if (!is.element(root, names(syberia_projects)))
      syberia_projects[[root]] <<- director(root, 'syberia')
    syberia_projects[[root]]
  }
})




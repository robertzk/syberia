#' Bootstrap a Syberia engine.
#'
#' A Syberia engine defines the core re-usable structural unit across
#' different Syberia projects. In the same way that
#' \href{http://guides.rubyonrails.org/engines.html}{Rails engines}
#' provide a modular structure for \href{Rails}{http://rubyonrails.org/}
#' projects, Syberia engines serve as the re-usable rockbed upon which
#' to construct projects that contain similar components.
#'
#' A Syberia engine is managed by a \code{\link[director]{director}} object.
#' This object ensures that the engine cannot access resources outside of
#' its domain, and allows insularity from other engines and the top-level
#' project from which the engine will be used.
#'
#' @param filepath character. The root directory of the engine. If this
#'    directory does not define a (relative) \code{"config/application.R"} 
#'    file, the parent directories of \code{filepath} will be traversed
#'    until such a file is found, or the function will error.
#' @export
#' @note The syberia package will maintain an internal cache of engines.
#'    Therefore, calling \code{syberia_engine} twice will retrieve the
#'    cached object. This cache is maintained in the \code{.syberia_env}
#'    environment object in the syberia package namespace.
#' @return The \code{\link[director]{director}} object responsible for
#'    managing the engine.
syberia_engine <- function(filepath) {
  traverse_parent_directories(normalizePath(filepath), function(filepath) {
    if (has_application_file(filepath)) {
      .syberia_env[[filepath]] <- .syberia_env[[filepath]] %||% build_engine(filepath)
    }
  }, error = sprintf("No syberia engine found at %s", sQuote(crayon::red(filepath))))
}

extensions <- c('.R', '.r', '/application.R', '/application.r')
has_application_file <- function(filepath) {
  any(file.exists(paste0(file.path(filepath, 'config', 'application'), extensions)))
}

build_engine <- function(filepath) {
  "Thomas" # the engine
}


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
#' @return The \code{\link[director]{director}} object responsible for
#'    managing the engine.
syberia_engine <- function(filepath) {

}


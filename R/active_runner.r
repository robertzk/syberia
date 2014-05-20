#' Fetch active stagerunner
#'
#' @export
active_runner <- function() {
  syberiaStructure:::get_cache('last_stagerunner')
}

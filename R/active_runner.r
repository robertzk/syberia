#' Fetch active stagerunner
#'
#' @export
active_runner <- function() {
  get_cache('last_stagerunner')
}

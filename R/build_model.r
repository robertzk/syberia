#' Build a model using a data source from scratch.
#' @param key a string. There must be a file with name \code{key}
#'   followed by '.r' so that syberia can read the model configurations.
#' @export
build_model <- function(key) {
  # TODO: Add path mechanism

  if (!file.exists(src_file <- pp("#{key}.r")))
    stop(pp("No file for model '#{key}'"))

  # parse source
  # old_dir <- getwd()
  # setwd(path)
  # on.exit(setwd(old_dir))

  stage_runner(source(src_file)$value)
}


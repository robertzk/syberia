#' Build a model using a data source from scratch.
#' @param path a string identifying the path where the model files reside.
build_model <- function(path = ".") {
  # parse source
  old_dir <- getwd()
  setwd(path)
  on.exit(setwd(old_dir))

  # fetch_data_from_source()
  source_options <- source("source.r")$value

  # prepare_data()
  preparation_procedure <- source("data.r")$value
  
  iris2 <- do.call(munge, append(list(dataframe), preparation_procedure))
  browser()
}


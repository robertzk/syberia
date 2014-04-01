#' Get syberia config relative to another source file.
get_registry_dir <- function(source_file) {
  prev_dir <- ""
  dir <- dirname(source_file)
  while(prev_dir != dir) {
    if (file.exists(file.path(dir, "syberia.config"))) {
      if (!file.exists(.syberia_dir <- file.path(dir, ".syberia")))
        dir.create(.syberia_dir) # create .syberia directory
      return(.syberia_dir)
    }
    prev_dir <- dir
    dir <- dirname(prev_dir)
  }
  stop("No syberia.config file found -- please create it in the root ",
       "of your syberia project.")
}

if ('memoise' %in% installed.packages()) {
  require(memoise)
  memoise(get_registry_dir)
}


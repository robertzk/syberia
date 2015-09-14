manual_box <- function(package_list) {
  invisible(
    lapply(package_list$packages, function(pkg) {
      ref <- if (is.null(pkg$ref)) pkg$version else pkg$ref
      if (!require(pkg$name, character.only=TRUE, quietly=TRUE) ||
        utils::packageVersion(pkg$name) != package_version(pkg$version)) {
          message('Installing ', pkg$name, ', version ', pkg$version)
          devtools::install_github(pkg$repo, ref = ref, subdir = pkg$subdir)
          if (pkg$unload == 'true') unloadNamespace(pkg$name)
      }
    })
  )
  invisible(TRUE)
}

packagefile <- function(file, ..., read = FALSE) {
  file <- system.file(file, ..., package = "syberia.io")
  if (isTRUE(read)) {
    paste(collapse = "\n", readLines(file))
  } else {
    file
  }
}

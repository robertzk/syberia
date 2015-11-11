# Check for the existence of READMEs
check_readme <- function(env) {
  # TODO: (RK) Would list.dirs be faster? Includes .git

  # All directories of non-test resources need READMEs
  resources <- gsub('^\\/', '', env$director$find(''))
  resources <- Filter(function(x) substring(x, 1, 6) != '/test/', resources)
  resources <- c(resources, dirname(resources))
  resources <- Filter(function(x) file.info(file.path(env$director$root(), x))$isdir, resources)
  resources <- unique(c(recursive = TRUE, lapply(resources, function(x)
    Reduce(file.path, strsplit(x, '/')[[1]], accumulate = TRUE))))

  readmes <- file.path(resources, 'README.md')
  readmes_exist <- vapply(readmes, function(x) file.exists(file.path(env$director$root(), x)), logical(1))
  missing_readmes <- names(which(!readmes_exist))

  if (length(missing_readmes) > 0) {
    stop(call. = FALSE, "The following directories are missing README.md files:\n\n",
         crayon::red(paste(missing_readmes, collapse = "\n")))
  }
}

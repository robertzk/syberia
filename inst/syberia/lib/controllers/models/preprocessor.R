# The models preprocessor.
#
# Inject lexicals (convenient syntax shortcuts) and "output" helper.
preprocessor <- function(source_env, director, resource) {
  source_env$model_version <- version <- gsub("^[^/]+\\/[^/]+\\/", "", resource)
  source_env$model_name    <- basename(version)
  source_env$output <-
    function(suffix = '', create = TRUE, dir = file.path(director$root(), 'tmp')) {
      filename <- file.path(dir, version, suffix)
      if (create && !file.exists(dir <- dirname(filename)))
        dir.create(dir, recursive = TRUE)
      filename
    }

  lexicals <- director$resource('lib/shared/lexicals')
  for (x in ls(lexicals)) source_env[[x]] <- lexicals[[x]]

  # Add mungebits to local environment.
  local_mungebits <- lapply(mungebits_names <- director$find(base = 'lib/mungebits'),
    function(x) director$resource(x))
  mungebits_names <- gsub('/', '.',
    sapply(mungebits_names, function(x) director:::strip_root('lib/mungebits', x)),
    fixed = TRUE)

  for (i in seq_along(mungebits_names)) {
    source_env[[mungebits_names[i]]] <- local_mungebits[[i]]
  }

  source()
}

cache <- TRUE

preprocessor <- function(source_env) {
  if ((name <- tolower(basename(resource))) %in% c('r', 's3', 'file'))
    syberiaStages:::fetch_adapter(name)
  else
    source()
}

function(input, output, resource, director) {
  # If an adapter does not specify a read and write function,
  # but returns a function, we assume this should be used instead of the
  # the default import stage.
  is_stage_generating_function <-
    (!exists('read',  env = input, inherits = FALSE) ||
     !exists('write', env = input, inherits = FALSE)) &&
    is.function(output)

  if (is(output, 'adapter') || is.stagerunner(output) ||
      is_stage_generating_function) return(output)

  # TODO: (RE) TEMPORARY HACK TO PREVENT ADAPTER CACHING.
  syberiaStructure:::set_cache(list(), 'adapters')

  adapters <- syberiaStructure:::get_cache('adapters') %||% list()
  adapter_name <- gsub("^lib/adapters/", "", resource)
  if (!is.element(adapter_name, names(adapters))) {
    # TODO: (RK) Temporary hack
    if (director:::is.idempotent_directory(file.path(director$root(), resource)))
      adapter_name <- file.path(adapter_name, basename(adapter_name))

    adapters[[adapter_name]] <- syberiaStages:::fetch_adapter(adapter_name)
    syberiaStructure:::set_cache(adapters, 'adapters')
  }

  adapters[[adapter_name]]
}

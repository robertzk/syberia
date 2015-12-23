# Import stage.

default_adapter <- resource('lib/shared/default_adapter')

#' Build a stagerunner for importing data with backup sources.
#'
#' @param modelenv environment. The modeling environment to use for
#'   constructing the import stage. Will be passed on to stage
#'   generating functions.
#' @param import_options list. Nested list, one adapter per list entry.
#'   These adapter parametrizations will get converted to legitimate
#'   IO adapters. (See the "adapter" reference class.)
build_import_stagerunner <- function(modelenv, import_options) {
  if (nzchar(Sys.getenv("CI"))) return(list("import" = force))

  stages <- Reduce(append, lapply(seq_along(import_options), function(index) {
    adapter_name <- names(import_options)[index] %||% default_adapter
    adapter_name <- gsub('.', '/', adapter_name, fixed = TRUE)
    adapter <- resource(file.path('lib', 'adapters', adapter_name))
    opts    <- import_options[[index]]

    if (is.function(adapter)) {
      # If a raw function, give it the import options and let it generate
      # the stage function. This is useful if you need finer control over
      # the importing process.
      setNames(list(adapter(modelenv, opts)), adapter_name)
    } else {
      setNames(list(function(modelenv) {
        # Only run if data isn't already loaded
        if (!'data' %in% ls(modelenv)) {
          modelenv$import_stage$adapter <- adapter
          modelenv$data <- adapter$read(opts)
        }
      }), adapter$.keyword)
    }
  }))

  if (length(stages) > 0)
    names(stages) <- vapply(names(stages), function(stage_name)
      paste0("Import from ", gsub('/', '.', as.character(stage_name),
                                  fixed = TRUE)), character(1))


  stages <- append(stages,
    list("(Internal) Verify data was loaded" = function(modelenv) {
      if (!'data' %in% ls(modelenv)) {
        stop("Failed to load data from all data sources", call. = FALSE)
      }

      modelenv$import_stage$env <-
        list2env(list(full_data = modelenv$data), parent = emptyenv())

      # TODO: (RK) Move this somewhere else.
      if (is.data.frame(modelenv$data)) {
        modelenv$import_stage$variable_summaries <-
          statsUtils::variable_summaries(modelenv$data)
      }
    })
  )

  stages
}

function(modelenv, import_options) {
  if (!is.list(import_options)) { # Coerce to a list using the default adapter
    import_options <- setNames(list(resource = import_options), default_adapter)
  }

  build_import_stagerunner(modelenv, import_options)
}

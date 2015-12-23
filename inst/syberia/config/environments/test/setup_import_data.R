# To test model data preparation procedures in continuous integration,
# we can't be running import stage, since this involves setting up
# this like a database connection to a back end and takes a long
# time. Instead, we store a snapshot of the first 100 ids and put that
# in a cache in this repository. This function checks that all such
# "import_data" caches are present for tested models.
function(director, optional_tests) {
  force(director); force(optional_tests)
  setup_import_data <- function(env) {
    models <- director$find('^models', method = 'substring')
    # TODO: (RK) Remove this hotfix when director$find doesn't return leading '/'
    models <- gsub("^\\/", "", models)

    test_data_registry <- file.path(director$root(), "test", ".registry", "import_data")
    models_with_test_data <- list.files(test_data_registry, recursive = TRUE)

    models_without_test_data <- setdiff(models, c(models_with_test_data, optional_tests))

    if (length(models_without_test_data) > 0) {
      cat(sep = '', crayon::yellow("Running data preparation"),
          " on the following models so they can be tested on Travis:\n",
          paste(paste(" *", models_without_test_data), collapse = "\n"), "\n\n")
      for (model in models_without_test_data) {
        # Run the test, which will automatically cache the import data.
        director$resource(file.path("test", model))$value()
      }
    }
  }
}

# Test a model by running its data preparation on 100 rows and ensure
# there are no errors.

preprocessor <- function() {
  library(testthat)
  library(testthatsomemore)
  tested_resource <- gsub("^test\\/", "", resource)
  context(tested_resource)

  colored_name <- function(color = 'red')
    sQuote(eval(bquote(`::`(crayon, .(as.name(color)))))(tested_resource))

  if (!director$exists(tested_resource)) {
    stop("There is a test for ", colored_name('red'), " but no actual model.", call. = FALSE)
  }

  model <- director$resource(tested_resource)$value(parse. = FALSE)
  if (!is.element('data', names(model))) { return() } # Skip if no data stage.

  message("Testing ", colored_name('green'), "\n")

  stagerunner <- suppressMessages(director$resource(tested_resource)$value(recompile = TRUE))
  if (!is.element('data', names(stagerunner$stages))) return()

  # TODO: (RK) Perform import data caching in the construct_stage_runner
  # function instead.
  reg <- director::registry(file.path(director$root(), 'test', '.registry'))
  import_data <- NULL

  if (!is.null(test_data <- reg$get('import_data', tested_resource)) || !interactive()) {
    import_data <- test_data
  } else {
    stagerunner$run('import')
    attr(stagerunner$.context$data, 'mungepieces') <- NULL
    row.names(stagerunner$.context$data) <- NULL
    num_rows <- NROW(stagerunner$.context$data)
    if (num_rows == 0) stop("Data from import stage has no rows", call. = FALSE)
    import_data <- stagerunner$.context$data[ sample(seq_len(num_rows), 100), ]
    reg$set(file.path('import_data', tested_resource), import_data)
    director$.cache$import_data[[tested_resource]] <- TRUE
  }

  stagerunner <- stagerunner$stages$data
  stagerunner$.context$data <- import_data

  stream <- tempfile()
  sink(stream)
  on.exit({ sink(); unlink(stream) }, add = TRUE)
  output <- tryCatch(error = identity,
    stagerunner$run(verbose = TRUE, remember_flag = FALSE))
  if (is(output, 'error')) {
    message("\nError when testing ", colored_name(), ": \n",
        paste(collapse = "\n", readLines(stream)), "\n\n",
        crayon::red$bold(output$message), "\n")
    # Re-trigger error
    stagerunner$run(verbose = TRUE, remember_flag = FALSE)
  }

  if (NROW(stagerunner$.context$data) == 0)
    stop("Munging removed all of the rows in ", colored_name(), call. = FALSE)
  if (NCOL(stagerunner$.context$data) == 0)
    stop("Munging removed all of the columns in ", colored_name(), call. = FALSE)

  invisible(TRUE)
}

# No test parser is necessary.
function() { }

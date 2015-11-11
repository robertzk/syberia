function(input) {
  force(input)
  function(munge_procedure = list(), default_args = list(), internal = list()) {
    container <- tundra:::tundra_container$new(resource, input$train, input$predict,
      munge_procedure, default_args, internal)
    if (!is.null(input$read) || 
        !is.null(input$write))
      attr(container, "s3mpi.serialize") <- list(read = input$read, write = input$write)
    container
  }
}

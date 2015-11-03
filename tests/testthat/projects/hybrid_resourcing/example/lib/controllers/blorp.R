function(output, input) {
  paste(output$blorp, resource(file.path("flump", input$flump)))
}


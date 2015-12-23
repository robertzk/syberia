# This function will be called to train the model
train <- function(dataframe) {
  args <- list(
    as.formula(input$formula),
    data = dataframe,
    family = binomial(logit)
  )

  output <<- list(model = do.call(glm, args))
}

# This function will be called on the created model to make the prediction
predict <- function(dataframe) {
  predict(output$model, newdata = dataframe)
}

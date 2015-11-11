# A trivial regression model on Iris

# A syberia model file is a nested list structure. Top-level lists are called
# stages. You can create your own stages by writing `lib/stages/my_stage.R`.
# A stage should return a [stagerunner](github.com/robertzk/stagerunner) object.

list(
  import = list(
    # File, R and s3 adapters ship by default. If you want to make a different adapter
    # you can define one by writing `lib/adapters/my_adapter.R`.
    #
    # This simple adapter just reads "iris" from the R GlobalEnv using the R adapter.
    R = "iris"
  ),

  # Data stage is a perfect place to transform your dataset prior to modeling
  # The default data stage defines a DSL for creating and training
  # [mungebits](github.com/robertzk/mungebits)
  # Yes, you need to train your data preparation!
  # Traditionally data scientists have been preparing models and shipping them to
  # engineers that would reimplement them in Java or another traditional server language.
  # This is a very slow and extremely error-prone process.
  #
  # Also, there is one more important consideration: data preparation should
  # operate differently in train versus predict!
  # For example, let's say that we want to impute a missing variable using column mean.
  # In training, you'd want to use the mean calculated from the import stage dataframe.
  # However, in production you do not have access to the input dataframe anymore!
  # So you need to store the imputed mean somewhere and use that number in production.
  # Data stage takes care of this duality, allowing you to use a plethora of mungebits
  # from [syberiaMungebits](github.com/robertzk/syberiaMungebits). Or you can write your own
  # and put them in `lib/mungebits/my_mungebit.R`
  data = list(
    # The left-hand side defines the informal name of a mungebit that you will see
    # when you run this model.
    # The right-hand side is the mungebit invocation.
    "Restore levels"       = list(restore_categorical_variables)
    # Lets try to use a GBM to predict whether Sepal.length will be greater than 6.
    ,"Create dep_var"       = list(new_variable, function(Sepal.Length) Sepal.Length > 6, "dep_var")
  ),

  # Once the data is prepared and is in the right format we are ready to
  # do the modeling itself.
  # You can use any R package to create a *classifier*.
  # Classifiers are determined by the `train` and `predict` functions.
  # The output of the model stage is a [tundraContainer](github.com/robertzk/tundra)
  # A tundracontainer is an object that contains all the information necessary
  # to make a prediction: the munge procedure, the classifier object, as well as
  # the ids of the variables that were in training. This helps to ensure that
  # you are not predicting on the same ids that you used for training,
  # helping you make a more accurate validation. You can set `.is_var` to the id column name
  # or it will default to 'id'.
  # The most interesting part about a tundracontainer is it's predict function.
  # The predict function first runs all the mungebits in predict mode,
  # then it checks that you are not predicting on train ids, and then calls the
  # classifier predict method, like `predict.gbm`
  model = list('gbm'
    , .id_var             = 'X'
    , distribution        = 'bernoulli'
    , number_of_trees     = 1000
    , shrinkage_factor    = 0.05
    , depth               = 4
    , min_observations    = 6
    , train_fraction      = 1
    , bag_fraction        = 0.5
    , cv                  = TRUE
    , cv_folds            = 5
    , number_of_cores     = 4
    , perf_method         = 'cv'
    , prediction_type     = 'response'
  ),

  # When all is said and done you need to export the result of your hard work.
  # This stage uses the same adapters as the *import* stage.
  # If you need to export to a custom place you need to write a new adapter and
  # implement the `write` function.
  export = list(
    s3 = 'syberia/example/gbm',
    R  = 'example_model'
  )
)

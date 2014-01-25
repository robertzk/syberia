define('const', function(const) {
(list(
  import = list(
    adapter = 's3',
    file = 'data/modeling_query_results_10_09_13_loan_purpose'
  ),
    
  data = list(
     "Rename default indicator"       = list( renamer,         c(initial_default_indicator  = 'dep_var'))
    ,"Dropping variables"             = list( drop_variables, const$bad_variables)
    ,"Parsing dates"                  = list( date_parser, const$date_cols    )
    ,"Making loan purpose lowercase"  = list( column_transformation(tolower), 'loan_purpose' )
    ,"Trimming loan purpose"          = list( column_transformation( function(x) str_extract(x, '^[^_ \\/]+') ), 'loan_purpose' )
    ,"Ordering by loan id"            = list( list(orderer, NULL), 'loan_id' )
   #, save
    ,"Setting loan purpose specials"  = list( value_replacer, 'loan_purpose', list(list('moving', 'other'), list('', NA_character_)))
    ,"Convert character to factors"   = list( column_transformation(factor), is.character )
    ,"Restore categorical variables"  = list( list(column_transformation(function(x) { inputs$levels <<- levels(x); x }, mutating = TRUE), column_transformation(function(x) { x <- factor(x, levels = inputs$levels); x }, mutating = TRUE)), is.factor )
    ,"Replacing score '999' value"    = list( value_replacer, 'score', list(list(999, NA_integer_)))
    ,"Dropping single-valued vars"    = list( drop_single_value_variables )
    ,"Imputing missing values"        = list( imputer, const$imputed_cols )
   #,"Computing income ratio"         = c( multi_column_transformation(function(income, median_income) 13 * income / median_income), c('income', 'median_income'), 'income_ratio')
    ,"Discretizing columns"           = list( discretizer, const$discretized_columns, granularity = 3, upper_count_bound = 15, lower_count_bound = 1) 
    ,"Remove identifying columns"     = list( drop_variables,  c("loan_id", "customer_id", "state", "source", "product_name"))
    ,"Replace NAs with Missing level" = list( value_replacer,  is.factor,      list(list(NA, 'Missing')))
    ,"Setup validation data"          = list( list(function(dataframe) eval(substitute({ dataframe <- dataframe[1:(0.8*nrow(dataframe)), ] }), envir = parent.frame()), NULL) )
    ,"Sure independence screening"    = list( list(function(dataframe) eval(substitute({
     do_not_discretize <-
       c("source", "loan_id", "dep_var", "state", "zip","clarity_report_id", "customer_id" ,"loan_purpose", "product_name", "credit_decision_id")
     tmp_cols <- colnames(dataframe)[vapply(dataframe, Negate(is.factor), logical(1))]
     tmp_cols <- setdiff(tmp_cols, do_not_discretize)
     tmp <- dataframe
     discretizer(tmp, tmp_cols, granularity = 3, upper_count_bound = NULL, lower_count_bound = 1)
     value_replacer(tmp, is.factor, list(list(NA, 'Missing')))
     sis <- new('driver.withSureIndependenceScreening', data = tmp, filter_bad_levels = FALSE)
     sis@data <- tmp
     sis <- sureIndependenceScreening(sis)
     inputs$cols <<- colnames(sis@data)
     dataframe <- dataframe[, colnames(sis@data)]
   }), envir = parent.frame()),
   function(dataframe) eval(substitute(dataframe <- dataframe[, inputs$cols]), envir = parent.frame())  ))
   , record("partial_data"), list(function(d) stop())
    #, list( function(dataframe) eval(substitute({
      # whatever code you want to modify dataframe
    #}), envir = parent.frame()))
  )

  , model = list(
    'gbm'
    , distribution        = 'bernoulli'
    , number_of_trees     = 100
    , shrinkage_factor    = 0.025
    , depth               = 4
    , min_observations    = 6
    , train_fraction      = 1
    , bag_fraction        = 0.5
    , cv                  = TRUE
    , cv_folds            = 10
    , number_of_cores     = 30
    , perf_method         = 'cv'
    , prediction_type     = 'response' 
  )
  
 
  , export = list(
    adapter = 's3',
    file = 'models/2.0.1_3'
  )
  
  , model2 = list(
    "ensemble",
    validation_buckets = 10,
    master = list(
      "gbm",
      data = list(
        "Normalize all scores" = list(function(dataframe) {
          # 
        })
      )
      , distribution        = 'bernoulli'
      , number_of_trees     = 100
      , shrinkage_factor    = 0.025
      , depth               = 4
      , min_observations    = 6
      , train_fraction      = 1
      , bag_fraction        = 0.5
      , cv                  = TRUE
      , cv_folds            = 10
      , number_of_cores     = 30
      , perf_method         = 'cv'
      , prediction_type     = 'response'
    ),
    submodels = list(
      "GBM family of models on 313 attributes" = 1 #replicate(50, model("3.0.0_gbm"))
      ,"GBM family of models on in-house vars"  = 1 #replicate(50, model("3.0.0_gbm"))
      ,"Discretized Logistic regression"        = 1 #model("3.0.0_logreg")
    )
  )
  
))})


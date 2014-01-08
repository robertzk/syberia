define('const', function(const) {
list(
  import = list(
    file = '~/avant-credit-model/data/modeling_query_results_10_09_13_loan_purpose.csv'
  ),
    
  data = list(
     "Rename default indicator"      = list( renamer,         c(initial_default_indicator = 'dep_var'))
    ,"Dropping variables"            = list( drop_variables,  const$bad_variables)
    ,"Parsing dates"                 = list( date_parser,     const$date_cols    )
    ,"Making loan purpose lowercase" = list( column_transformation(tolower), 'loan_purpose' )
    ,"Ordering by loan id"           = list( list(orderer, NULL),         'loan_id'    )
   #, save
    ,"Setting loan purpose specials" = list( value_replacer,  'loan_purpose',  list(list('moving', 'other'), list('', NA_character_)))
    ,"Replacing score '999' value"   = list( value_replacer,  'score',         list(list(999, NA_integer_)))
    ,"Dropping single-valued vars"   = list( drop_single_value_variables   )
    ,"Imputing means"                = list( imputer,         const$imputed_cols )
   #,"Computing income ratio"        = c( multi_column_transformation(function(income, median_income) 13 * income / median_income), c('income', 'median_income'), 'income_ratio')
    ,"Discretizing columns"          = list( discretizer,     const$discretized_columns, granularity = 4, lower_count_bound = 15)
    ,"Remove identifying columns"    = list( drop_variables,  c("loan_id", "customer_id", "state", "source", "product_name", "loan_purpose"))
  ),

  model = list(
     "gbm"
   , distribution        = 'bernoulli'
   , number_of_trees     = 100
   , shrinkage_factor    = 0.025
   , depth               = 4
   , min_observations    = 6
   , train_fraction      = 1
   , bag_fraction        = 0.5
   , cv                  = TRUE
   , cv_folds            = 10
   , number_of_cores     = 6
   , perf_method         = 'cv'
   , prediction_type     = 'response'
  ),

  export = list(
    adapter = 's3',
    file = 'models/2.0.1'
  )
)})

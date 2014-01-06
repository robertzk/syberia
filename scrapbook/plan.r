munge_procedure <- list(
   "Rename default indicator"      = list( renamer,         c(initial_default_indicator = 'dep_var'))
  ,"Dropping variables"            = list( drop_variables,  bad_variables)
  ,"Parsing dates"                 = list( date_parser,     date_cols    )
  ,"Making loan purpose lowercase" = list( column_transformation(tolower), 'loan_purpose' )
  ,"Setting loan purpose specials" = list( value_replacer,  'loan_purpose',  list(list('moving', 'other'), list('', NA_character_)))
  ,"Replacing score '999' value"   = list( value_replacer,  'score',         list(list(999, NA_integer_)))
  ,"Dropping single-valued vars"   = list( drop_single_value_variables   )
  ,"Imputing means"                = list( imputer,         imputed_cols )
 #,"Computing income ratio"        = c( multi_column_transformation(function(income, median_income) 13 * income / median_income), c('income', 'median_income'), 'income_ratio')
  ,"Ordering by loan id"           = list( list(orderer, NULL),         'loan_id'    )
  ,"Discretizing columns"          = list( discretizer,     discretized_columns, granularity = 4, lower_count_bound = 15)
)

processed_train_data <- munge(raw_data, munge_procedure)
# raw_predict_data <- raw_data[1:2, ]
# processed_predict_data <- munge(raw_predict_data, processed_train_data)
# raw_predict_data[, 20:30]
# processed_predict_data[, colnames(raw_predict_data)[20:30]]

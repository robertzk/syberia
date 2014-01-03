munge_procedure <- list(
   "Dropping variables"            = list( drop_variables,  bad_variables)
  ,"Parsing dates"                 = list( date_parser,     date_cols    )
  ,"Making loan purpose lowercase" = list( column_transformation(tolower), 'loan_purpose' )
  ,"Setting loan purpose specials" = list( value_replacer,  'loan_purpose',  list(list('moving', 'other'), list('', NA_character_)))
  ,"Replacing score '999' value"   = list( value_replacer,  'score',         list(list(999, NA_integer_)))
  ,"Dropping single-valued vars"   = list( drop_single_value_variables   )
  ,"Imputing means"                = list( imputer,         imputed_cols )
 #,"Computing income ratio"        = c( multi_column_transformation(function(income, median_income) 13 * income / median_income), c('income', 'median_income'), 'income_ratio')
  ,"Ordering by loan id"           = list( list(orderer, column_transformation(force)),         'loan_id'    )
  ,"Discretizing columns"          = list( discretizer,     discretized_columns, granularity = 3)
)
# Next steps:
# <model>_file.r    - Persistent file preprocessing
# <model>_data.r    - Data preprocessing using munge, basically this file
# <model>_model.r   - Model specifications...not sure how to do this yet
# <model>_export.r  - Saving and validation specs...also not sure how to do this yet
# <model>_test.r    - Testing the previous files. Using things like after("Dropping variables", { expect_that(...) })

# dd2 <- munge(dd, munge_procedure)
# dd1 <- dd[1:2, ]
# dd3 <- munge(dd1, dd2)

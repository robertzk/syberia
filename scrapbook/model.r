gbm_args <- list(
   "gbm"
 , distribution        = 'bernoulli'
 , number_of_trees     = 100
 , shrinkage_factor    = 0.001
 , depth               = 4
 , min_observations    = 6
 , train_fraction      = 1
 , bag_fraction        = 0.5
 , cv                  = TRUE
 , cv_folds            = 10
 , n.cores             = 6
 , perf_method         = 'cv'
 , prediction_type     = 'response'
)

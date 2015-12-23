# In order to create a mungebit you need to write a `train` and `predict`
# function. Mungebits controller will create an object out of those that is
# useable for model training.

# This mungebit just creates a factor variable of possible titles
# No train/predict problems here, so train is identical to predict.

predict <- train <- function(data) {
  # Notice the eval.parent(substitute(...)) hack.
  # This was important for R versions prior to 3.1 so that we do not copy
  # large dataframes.
  # These days it is still useful in conjunction with
  # [objectdiff](github.com/robertzk/objectdiff)
  # This package allows you to store diffs of the modeling environment,
  # allowing you to debug your data preparation.
  eval.parent(substitute({
    data$title <- factor(ifelse(data$is_mister, "mr",
                         ifelse(data$is_missus, "mrs",
                         ifelse(data$is_miss, "ms",
                         ifelse(data$is_master, "master",
                         ifelse(data$is_rev, "rev",
                         ifelse(data$is_dr, "dr", "other")))))))
  }))
}

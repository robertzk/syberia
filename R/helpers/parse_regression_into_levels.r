if (!exists('filter_level_name')) source('src/helpers/filter_level_name.r')

parse_regression_into_levels <-
  function(regression_coefficients, column, variable, indep_vars, active_vars, reject_coef) {
    good_levels <- c()
    bad_levels <- c()
    worst_level <- ''
    worst_level_pval <- 0

    for(row_index in 2:dim(regression_coefficients)[1]) {
      level_name <- rownames(regression_coefficients)[row_index]
      relevant_levels <- if (is.factor(column)) levels(column) else 1:3
      # ^ TODO: REALLY BAD attr(col, 'degree')

      if (is.factor(column) || !is.numeric(column)) {
        level_name <- filter_level_name(level_name, variable,
                                        union(variable, indep_vars),
                                        #active_vars,
                                        relevant_levels)
      } else {
        var_name <- paste("ns(", variable, ", df = 3)", sep = '')
        level_name <- filter_level_name(level_name, var_name,
                                        active_vars, relevant_levels)
      }

      if (level_name == FALSE) next
      p_value <- regression_coefficients[row_index, 4] 
      if (p_value <= reject_coef) {
        good_levels <- append(good_levels, level_name)
      } else {
        if (worst_level == '' || worst_level_pval < p_value) {
          worst_level <- level_name
          worst_level_pval <- p_value
        }
        bad_levels <- append(bad_levels, level_name)
      }
    }

    list(good_levels = good_levels, bad_levels = bad_levels, worst_level = worst_level)
  }

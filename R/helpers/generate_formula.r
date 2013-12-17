generate_formula <- function(dep_var, indep_vars) {
  if (length(indep_vars) == 0) indep_vars <- 'NULL'
  else if (length(indep_vars) > 1) indep_vars <- paste(indep_vars, collapse = '+')
  formula(paste(dep_var, '~', indep_vars))
}

#' Help determine whether a concatenated level name in a 
#' regression summary comes from a specific variable.
#'
#' @param level_name character. Name of the current level
#' @param correct_varname character. What the variable name should be
#' @param vars list. A list of the input variables
#' @param level_names list. A list of the names of the levels

filter_level_name <- function(level_name, correct_varname, vars, level_names) {
  for(varname in vars) {
    if (substr(level_name, 0, nchar(varname)) == varname) {
      cleaned_level_name <- substr(level_name, nchar(varname) + 1, nchar(level_name))
      if (!(cleaned_level_name %in% level_names)) next
      if (varname != correct_varname) return(FALSE)
      return(cleaned_level_name)
    }
  }
  FALSE
}

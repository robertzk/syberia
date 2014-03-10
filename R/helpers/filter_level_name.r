#' Cleans up each level name
#'
#' @param level_name: name of the current level
#' @param correct_varname: what the varname should be
#' @param vars: list of variables
#' @param level_names: list of the levels

filter_level_name <- function(level_name, correct_varname, vars, level_names) {
  lapply(vars,function(varname){
    if (substr(level_name, 0, nchar(varname)) == varname) {
      cleaned_level_name <- substr(level_name, nchar(varname) + 1, nchar(level_name))
      if (!(cleaned_level_name %in% level_names)) return
      if (varname != correct_varname) return(FALSE)
      return(cleaned_level_name)
    }
  })
  FALSE
}

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

(function() {
  extract_ranges_from_levels <<- function(levs) {
    lapply(levs, function(lev) {
      lev <- as.character(lev)
      regex_result <- gregexpr('\\s*(\\[|\\()\\s*([\\d\\.-]*)\\s*,\\s*([\\d\\.-]*)\\s*(\\]|\\))\\s*', lev, perl = TRUE)[[1]]
      if (attr(regex_result, 'match.length') == -1) {
        old_opts <- options(warn = -1)
        on.exit(options(old_opts))
        return(lev)
      }

      starts <- attr(regex_result, 'capture.start')
      lengths <- attr(regex_result, 'capture.length')
      regex_output <- sapply(1:4, function(index) {
        substr(lev, starts[index], starts[index] + lengths[index] - 1)
      })
      left_bound <- as.numeric(regex_output[2])
      right_bound <- as.numeric(regex_output[3])
      left_operator <- if (regex_output[1] == '(') `<` else `<=`
      right_operator <- if (regex_output[4] == ')') `>` else `>=`
      list(left_bound = left_bound, right_bound = right_bound,
           left_operator = left_operator, right_operator = right_operator)
    })
  }
})()

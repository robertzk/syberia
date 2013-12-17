(function() {
  source('src/helpers/extract_ranges_from_levels.r')
  source('src/helpers/pp.r')

  # extract_operator(`<=`) gives '<='
  extract_operator <- function(op) {
   result <- regmatches(head(op)[1], gregexpr('"([^"]+)"', head(op)[1],
                        perl = TRUE))[[1]]
   str_replace(str_replace(result, '"', ''), '"', '')
  }

  parse_factors_into_query <<- function(factor_list, coefficients_matrix) {
    query <- character(0)
    sapply(names(factor_list), function(fact) {
      levs <- factor_list[[fact]]
      sapply(levs, function(lev) {
        var_name <- paste(fact, lev, sep = '') 
        if (var_name %in% coefficients_matrix[, 1]) {
          coeff <- coefficients_matrix[coefficients_matrix[, 1] == var_name, 2]
          ranges <- extract_ranges_from_levels(lev)[[1]] 
          if (is.character(ranges)) {
            old_opts <- options(warn = -1)
            if (ranges == 'Missing') {
              query <<- append(query, pp(
                "case when #{fact} IS NULL then #{coeff} else 0 end"
              ))
            } else if (!is.na(as.numeric(ranges))) {
              # numeric level, e.g., 2 in c('[1,2)', '2', '(2,3]')
              options(old_opts)
              query <<- append(query, pp(
                "case when #{fact}::varchar::numeric = #{ranges} then #{coeff} else 0 end"
              ))
            } else {
              options(old_opts)
              # character level, e.g., 'foo' in c('foo', 'bar', 'baz')
              query <<- append(query, pp(
                "case when #{fact}::varchar = '#{ranges}' then #{coeff} else 0 end"
              ))
            }
          } else {
            # range level
            left_bd <- ranges$left_bound
            right_bd <- ranges$right_bound
            left_op <- extract_operator(ranges$left_operator)
            right_op <- extract_operator(ranges$right_operator)
            query <<- append(query, pp(
              "case when #{left_bd} #{left_op} #{fact}::varchar::numeric AND ",
              "#{right_bd} #{right_op} #{fact}::varchar::numeric then #{coeff} else 0 end"
            ))
          }
        }
      })
    })
    query <- append(
      as.character(coefficients_matrix[coefficients_matrix[,1] == 'intercept', 2])
    , query)

    paste(query, collapse = " + \n")
  }
})()

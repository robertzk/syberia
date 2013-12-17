# String interpolation in R!
pp <- function(..., envir = parent.frame(), sep = '', collapse = '') {
  string <- list(...)
  if (length(string) > 1)
    return(paste(sapply(string,
      function(s) { pp(s, envir = envir, sep = sep, collapse = collapse) }
    ), collapse = sep))
  string <- string[[1]]
  if (length(string) > 1)
    return(paste(sapply(string,
      function(s) { pp(s, envir = envir, sep = sep, collapse = collapse) }
    ), collapse = collapse))
  regex <- gregexpr('#\\{([^\\}]+)\\}', string, perl=TRUE)
  starts <- attr(regex[[1]], 'capture.start')
  lengths <- attr(regex[[1]], 'capture.length')
  buildstr <- ''
  last <- 1
  for(i in 1:length(attr(regex[[1]], 'capture.start'))) {
    buildstr <- append(buildstr,
      c(substr(string, last, starts[i] - 3),
        eval(parse(text = substr(string, starts[i], starts[i] + lengths[i] - 1)),
        envir = envir)
       ))
    
    last <- starts[i] + lengths[i] + 1
  }
  buildstr <- append(buildstr, substr(string, last, nchar(string)))
  paste(buildstr, collapse = '')
}

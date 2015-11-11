# The data controller.
preprocessor <- function(director, source_args) {
  # Add lexicals to local environment.
  lexicals <- director$resource('lib/shared/lexicals')$value()
  for (x in ls(lexicals)) source_args$local[[x]] <- lexicals[[x]]

  # Add mungebits to local environment.
  mungebits <- lapply(mungebits_names <- director$find(base = 'lib/mungebits'),
    function(x) director$resource(x)$value())
  mungebits_names <- gsub('/', '.',
    sapply(mungebits_names, function(x) director:::strip_root('lib/mungebits', x)),
    fixed = TRUE)

  for (i in seq_along(mungebits))
    source_args$local[[mungebits_names[i]]] <- mungebits[[i]]

  source()
}

function(director, args) {
  if (isTRUE(args$raw)) output #Return a list of munge_procedures
  else {
    data_stage <- director$resource('lib/stages/data')$value()
  
    data_stage(new.env(), output, remember = !identical(args$remember, FALSE))
  }
}


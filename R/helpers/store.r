(function() {
  newtoR('AWS.tools')
  newtoR('stringr')
  path <- 's3://avantminer/tmp/'

  grab_latest_file_in_s3_dir <<- function(.path = path) {
    paths <- system(paste('s3cmd ls ', .path, '*', sep = ''), intern = TRUE)
    times <- as.POSIXct(substring(paths, 1, 16))
    latest <- which(max(times) == times)
    regex <- paste(str_replace(.path, '\\/', '\\\\/'), '(.+)', sep = '')
    results <- gregexpr(regex, paths, perl = TRUE)
    substring(regmatches(paths, results)[[latest[1]]], 1 + nchar(.path))
  }

  store <<- function(obj, name = NULL, .path = path) {
    if (is.null(name)) name = deparse(substitute(obj))
    s3.put(obj, paste(.path, name, sep = ''))
  }

  read <<- function(name = NULL, .path = path) { 
    if (is.null(name)) name <- grab_latest_file_in_s3_dir(.path)
    s3.get(paste(.path, name, sep = ''))
  }
})()

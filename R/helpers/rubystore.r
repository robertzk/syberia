(function() {
  source('src/helpers/store.r')
  newtoR('rjson')
  path = 's3://avantminer/rubympi/'

  s3_path <- function(path, name) {
    paste('"', path, name, '"', sep = '')
  }

  rubystore <<- function(obj, name = NULL) {
    if (is.null(name)) name = deparse(substitute(obj))
    x.serialized <- tempfile()
    write(toJSON(obj), x.serialized)
    s3.cmd <- paste("s3cmd put", x.serialized, s3_path(path, name))
    res <- system(s3.cmd, intern = TRUE)
    unlink(x.serialized)
    res
  }

  rubyread <<- function(name = NULL) {
    if (is.null(name)) name <- grab_latest_file_in_s3_dir(path)
    x.serialized <- tempfile()
    s3.cmd <- paste("s3cmd get", s3_path(path, name), x.serialized)
    res <- system(s3.cmd, intern = TRUE)
    ans <- fromJSON(paste(readLines(x.serialized), collapse = "\n"))
    unlink(x.serialized)
    ans
  }
})()

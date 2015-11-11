read <- function(name) {
  if (director$cache$exists(name$resource)) {
    message('Reading from cache...')
    director$cache$get(name$resource)
  } else {
    temp <- tempfile(); on.exit(unlink(temp))
    download.file(name$resource, temp, method = 'curl')
    message('reading into memory...')
    data <- readr::read_csv(unz(temp, gsub('.zip$', '', basename(name$resource))), col_names = FALSE)
    director$cache$set(name$resource, data)
    data
  }
}

write <- function(df) stop('Cannot write to a URL, aborting')

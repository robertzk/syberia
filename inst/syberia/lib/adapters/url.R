read <- function(name) {
  read.csv(text = RCurl::getURL(name$resource))
}

write <- function(df) stop('Cannot write to a URL, aborting')

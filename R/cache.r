#' Simple caching mechanism
#' http://r.789695.n4.nabble.com/what-is-the-preferred-method-to-create-a-package-local-variable-tp923040p923052.html
cache <- function() {
  .cache <- list()

  list(
    get = function(key = NULL) {
      if (is.null(key)) .cache
      else .cache[[key]]
    },
    getNames = function() names(.cache),
    set = function(value, key = NULL) {
      if (is.null(key)) .cache <<- value
      else .cache[[key]] <<- value
    }
  )
}

.syberia_cache <- cache()
set_cache <- function(value, key = NULL) .syberia_cache$set(value, key)
get_cache <- function(key = NULL) .syberia_cache$get(key)
get_cache_names <- function() .syberia_cache$getNames()


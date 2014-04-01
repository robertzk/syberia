get_registry_key <- function(key, registry_dir) {
  key <- sanitize_registry_key(key)
  if (!file.exists(filename <- file.path(registry_dir, key)))
    stop('There is no Syberia registry item with key "', key, '"')
  (readRDS(filename)) # do not use default invisibility
}

set_registry_key <- function(key, value, registry_dir) {
  key <- sanitize_registry_key(key)
  filename <- file.path(registry_dir, key)
  saveRDS(value, filename)
  key
}

sanitize_registry_key <- function(key) {
  if (grepl('..', key, fixed = TRUE))
    stop('Syberia registry keys cannot contain two consecutive dots')
  key
}


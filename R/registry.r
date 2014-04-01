get_registry_key <- function(key, registry_dir) {
  key <- sanitize_registry_key(key, registry_dir)
  (readRDS(filename)) # do not use default invisibility
}

set_registry_key <- function(key, value, registry_dir) {
  key <- sanitize_registry_key(key, registry_dir, read = FALSE)
  filename <- file.path(registry_dir, key)
  saveRDS(value, filename)
  key
}

sanitize_registry_key <- function(key, registry_dir, read = TRUE) {
  if (grepl('..', key, fixed = TRUE))
    stop('Syberia registry keys cannot contain two consecutive dots')

  if (read) {
    if (!file.exists(filename <- file.path(registry_dir, key)))
      stop('There is no Syberia registry item with key "', key, '"')
  } else {
    if ((dir <- dirname(key)) != '.') {
      tryCatch(dir.create(file.path(registry_dir, dir), recursive = TRUE),
               warning = function(e) {
                 if (grepl("reason 'Not a directory'", e$message))
                   stop('Cannot create Syberia registry key "', key, '"')
               })
    }
  }
  key
}


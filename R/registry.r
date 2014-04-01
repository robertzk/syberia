get_registry_key <- function(key, registry_dir) {
  filename <- sanitize_registry_key(key, registry_dir)
  (readRDS(filename)) # do not use default invisibility
}

set_registry_key <- function(key, value, registry_dir) {
  filename <- sanitize_registry_key(key, registry_dir, read = FALSE)
  tryCatch(saveRDS(value, filename), error = function(e)
           stop('Failed to save Syberia registry key "', key, "' because: ", e$message))
  key
}

sanitize_registry_key <- function(key, registry_dir, read = TRUE) {
  if (grepl('..', key, fixed = TRUE))
    stop('Syberia registry keys cannot contain two consecutive dots')

  if (read) {
    if (!file.exists(filename <- file.path(registry_dir, key)))
      stop('There is no Syberia registry item with key "', key, '"')
    else if (file.info(filename)$isdir)
      stop('There is no Syberia registry item with "', key, '", ',
           'because this key points to a directory.')
    filename
  } else {
    if ((dir <- dirname(key)) != '.') {
      tryCatch(dir.create(file.path(registry_dir, dir), recursive = TRUE),
               warning = handler <- function(e) {
                 if (grepl("reason 'Not a directory'", e$message))
                   stop('Cannot create Syberia registry key "', key, '"')
               })
    }
    file.path(registry_dir, key)
  }
}


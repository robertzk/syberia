engine("base", type = "local",
       path = system.file(file.path("engines", "base.sy"), package = "syberia"),
       mount = TRUE)

engine("utility1", type = "local", path =
       normalizePath(file.path(root(), "..", "utility1")), mount = FALSE)


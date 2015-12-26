engine("example", type = "local",
       path = normalizePath(file.path(root(), "..", "example")), mount = TRUE)

engine("base", type = "local",
       path = system.file(file.path("engines", "base.sy"), package = "syberia"),
       mount = TRUE)

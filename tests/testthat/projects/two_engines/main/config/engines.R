engine("engine1", type = "local",
       path = normalizePath(file.path(root(), "..", "engine1")), mount = TRUE)

engine("engine2", type = "local",
       path = normalizePath(file.path(root(), "..", "engine2")), mount = TRUE)

engine("engine1", type = "local",
       path = normalizePath(file.path(root(), "..", "engine1")), mount = TRUE)
engine("addon1", type = "local",
       path = normalizePath(file.path(root(), "..", "addon1")), mount = TRUE)

engine("example", type = "local",
       path = normalizePath(file.path(root(), "..", "example")), mount = TRUE)

engine("utils", type = "local",
       path = normalizePath(file.path(root(), "..", "utils")), mount = TRUE)


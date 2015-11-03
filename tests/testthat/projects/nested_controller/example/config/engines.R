engine("base", type = "local",
       path = system.file(file.path("engines", "base.sy"), package = "syberia"),
       mount = TRUE)

.onAttach <- function(parent_engine) {
  # Mount the routes on the parent engine.
  routes <- director$.engines$base$engine$resource("lib/controllers/routes", parent. = FALSE)

  parent_engine$register_parser("config/routes", routes$parser, overwrite = TRUE)
  parent_engine$resource("config/routes")
}


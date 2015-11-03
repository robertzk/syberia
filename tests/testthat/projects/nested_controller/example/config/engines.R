engine("base", type = "local",
       path = system.file(file.path("engines", "base.sy"), package = "syberia"),
       mount = TRUE)

.onAttach <- function(parent_engine) {
  # Fetch the routes from *this* engine.
  #routes <- director$resource("config/routes", parent. = FALSE, parse. = FALSE)

  #routes_controller <- director$resource("lib/controllers/routes")$parser
  #routes_controller$parser(director = parent_engine, 
  #  output = routes, any_dependencies_modified = FALSE,
  #  args = list(force = TRUE))
    # `director` is the base engine object.
  routes <- director$.engines$base$engine$resource("lib/controllers/routes", parent. = FALSE)

  parent_engine$register_parser("config/routes", routes$parser, overwrite = TRUE)
  parent_engine$resource("config/routes")
}


#' Bootstrap a Syberia engine.
#'
#' A Syberia engine defines the core re-usable structural unit across
#' different Syberia projects. In the same way that
#' \href{http://guides.rubyonrails.org/engines.html}{Rails engines}
#' provide a modular structure for \href{Rails}{http://rubyonrails.org/}
#' projects, Syberia engines serve as the re-usable rockbed upon which
#' to construct projects that contain similar components.
#'
#' A Syberia engine is managed by a \code{\link[director]{director}} object.
#' This object ensures that the engine cannot access resources outside of
#' its domain, and allows insularity from other engines and the top-level
#' project from which the engine will be used.
#'
#' @param filepath character. The root directory of the engine.
#'    If this directory does not define a (relative) \code{"config/application.R"} 
#'    file, the parent directories of \code{filepath} will be traversed
#'    until such a file is found, or the function will error.
#' @param ... Additional arguments used internally.
#' @export
#' @note The syberia package will maintain an internal cache of engines.
#'    Therefore, calling \code{syberia_engine} twice will retrieve the
#'    cached object. This cache is maintained in the \code{.syberia_env}
#'    environment object in the syberia package namespace.
#' @return The \code{\link[director]{director}} object responsible for
#'    managing the engine.
syberia_engine <- function(filepath, ...) {
  UseMethod("syberia_engine")
}

#' @export
syberia_engine.pre_engine <- function(filepath, ...) {
  build_engine(filepath)
}

#' @export
syberia_engine.character <- function(filepath, ...) {
  syberia_engine_character(filepath, ...)
}

syberia_engine_character <- function(filepath, cache = TRUE) {
  traverse_parent_directories(normalizePath(filepath), function(filepath) {
    if (isTRUE(cache) && has_application_file(filepath)) {
      .syberia_env[[filepath]] <- .syberia_env[[filepath]] %||% build_engine(filepath)
    } else if (has_application_file(filepath)) {
      build_engine(filepath)
    }
  }, error = sprintf("No syberia engine found at %s", sQuote(crayon::red(filepath))))
}

extensions <- c('.R', '.r', '/application.R', '/application.r')
has_application_file <- function(filepath) {
  any(file.exists(paste0(file.path(filepath, 'config', 'application'), extensions)))
}

#' Build a syberia engine.
#'
#' @param buildable character. A file path from which to build the engine.
#' @export
build_engine <- function(buildable) {
  UseMethod("build_engine")
}

#' @export
build_engine.pre_engine <- function(buildable) {
  dir <- engine_dir(buildable$prefix)
  if (!file.exists(dir)) buildable$builder(dir)
  syberia_engine(dir, cache = FALSE)
}

#' @export
build_engine.character <- function(buildable) {
  # bootstrap_engine(director::director(buildable))
  bootstrap_engine(syberia_engine_class$new(buildable))
}

engine_dir <- function(dir) {
  file.path(engine_location(), dir)
}

engine_location <- function() {
  path <- engine_location_path()
  if (!file.exists(path)) {
    if (!dir.create(path, FALSE, TRUE)) {
      stop(sprintf(paste0("Syberia needs a directory in which to place the code for ",
           "dependencies. Please ensure %s is writable, or set a ",
           "different path in the %s environment variable or ",
           "the %s global option (using %s)."),
           sQuote(crayon::red(path)),
           sQuote(crayon::yellow("SYBERIA_ENGINE_LOCATION")),
           sQuote(crayon::yellow("syberia.engine_location")),
           sQuote(crayon::magenta("options(syberia.engine_location = 'some/dir')"))))
    }
  }
  path
}

engine_location_path <- function() {
  Sys.getenv("SYBERIA_ENGINE_LOCATION") %|||%
  getOption("syberia.engine_location", "~/.R/.syberia/engines")
}

bootstrap_engine <- function(engine) {
  if (isTRUE(engine$cache_get("bootstrapped"))) return(engine)
  engine$register_preprocessor('config/boot',    boot_preprocessor)
  engine$register_preprocessor('config/engines', engine_preprocessor)
  engine$register_parser      ('config/engines', engine_parser)
  
  exists <- function(...) engine$exists(..., parent. = FALSE, children. = FALSE)
  if (exists("config/engines")) engine$resource("config/engines")
  if (exists("config/boot"))    engine$resource("config/boot")
  # Check for duplicate resources in mounted child engines.
  engine$find(check_duplicates. = TRUE, children. = TRUE, tag_engine. = TRUE)
  engine$cache_set("bootstrapped", TRUE)
  engine
}

boot_preprocessor <- function(source, source_env, director) {
  source_env$director <- director
  source()
}

engine_preprocessor <- function(source, source_env, preprocessor_output) {
  preprocessor_output$engines <- new.env(parent = emptyenv())
  source_env$engine <- function(name, ...) {
    preprocessor_output$engines[[name]] <- list(...)
  }
  source()
}

engine_parser <- function(director, preprocessor_output) {
  if (isTRUE(director$cache_get("bootstrapped"))) return()

  for (engine in ls(preprocessor_output$engines, all = TRUE)) {
    register_engine(director, engine, parse_engine(preprocessor_output$engines[[engine]]),
                    mount = isTRUE(preprocessor_output$engines[[engine]]$mount))
  }

  ## When we use `devtools::load_all` on director, it loads a symbol called
  ## `exists`; we use explicit base namespacing to prevent conflicts during development.
  if (base::exists(".onAttach", envir = input, inherits = FALSE)) {
    onAttach <- input$.onAttach
    environment(onAttach) <- list2env(
      list(director = director),
      parent = environment(onAttach)
    )
    director$cache_set(".onAttach", onAttach)
  }
  NULL
}

register_engine <- function(director, name, engine, mount = FALSE) {
  # TODO: (RK) Replace with $engines private member after R6ing.
  if (!director$cache_exists("engines")) {
    director$cache_set("engines", new.env(parent = emptyenv()))
  }
  env <- director$cache_get("engines")
  env[[name]] <- engine

  director$register_engine(name, engine, mount = mount)

  if (engine$cache_exists(".onAttach")) {
    engine$cache_get(".onAttach")(director)
  }
}

parse_engine <- function(engine_parameters) {
  engine_parameters$type <- engine_parameters$type %||% "github"

  if (!is.simple_string(engine_parameters$type)) {
    stop(sprintf("When defining an engine, please provide a string for the %s",
                 sQuote("type")), call. = FALSE)
  }

  parser <- paste0("parse_engine.", engine_parameters$type)
  ## When we use `devtools::load_all` on director, it loads a symbol called
  ## `exists`; we use explicit base namespacing to prevent conflicts during development.
  if (!base::exists(parser, envir = getNamespace("syberia"), inherits = FALSE)) {
    stop(sprintf("Cannot load an engine of type %s", 
                 sQuote(crayon::red(engine_parameters$type))))
  }
  syberia_engine(get(parser, envir = getNamespace("syberia"))(engine_parameters),
                 cache = FALSE)
}

parse_engine.github <- function(engine_parameters) {
  repo    <- engine_parameters$repo %||% engine_parameters$repository
  version <- engine_parameters$version %||% "master"
  stopifnot(is.simple_string(repo))

  pre_engine(prefix = file.path("github", repo, version),
    builder = function(filepath) {
      status <- system2("git", c("clone", sprintf("git@github.com:%s", repo), filepath))
      stopifnot(status == 0)
    })
}

parse_engine.local <- function(engine_parameters) {
  path <- engine_parameters$path %||% stop("Please provide an engine path")
  if (!file.exists(path)) stop("The path ", sQuote(path), " does not exist.")
  path
}

pre_engine <- function(prefix, builder) {
  structure(list(prefix = prefix, builder = builder), class = "pre_engine")
}
#
#syberia_engine_instance <- function(director) {
#  structure(list(director = director), class = "syberia_engine")
#}
#

#`$.syberia_engine` <- function(engine, method) {
#  if (identical(method, "exists")) syberia_engine_exists(engine)
#  else if (identical(method, "resource")) syberia_engine_resource(engine)
#  else eval.parent(bquote(`$`(.(substitute(engine))[['director']], .(method))))
#}
#
#syberia_engine_exists <- function(engine) {
#  force(engine)
#  function(...) {
#    if (!engine[['director']]$exists(...)) {
#      for (subengine in ls(engine[['director']]$cache_get("engines"), all = TRUE)) {
#        if (engine[['director']]$cache_get("engines")[[subengine]]$exists(...)) {
#          return(TRUE)
#        }
#      }
#    }
#    FALSE
#  }
#}
#
#syberia_engine_resource <- function(engine) {
#  force(engine)
#  function(name, ...) {
#    if (!engine[['director']]$exists(name)) {
#      for (subengine in ls(engine[['director']]$cache_get("engines"), all = TRUE)) {
#        subdirector <- engine[['director']]$cache_get("engines")[[subengine]]
#        if (subdirector$exists(name)) {
#          return(subdirector$resource(name, ...))
#        }
#      }
#    }
#    engine$resource(name, ...)
#  }
#}
#
#print.syberia_engine <- function(x, ...) {
#  print(x$director)
#}

#' Whether to exclude a syberia engine from being used for resourcing.
#'
#' @param condition logical. Some condition.
#' @param engine syberia_engine. Engine object.
#' @export
should_exclude <- function(condition, engine) {
  UseMethod("should_exclude")
}

#' @export
should_exclude.syberia_engine <- function(...) { identical(...) }

#' @export
should_exclude.character <- function(condition, engine) {
  identical(condition, engine$root())
}

syberia_engine_class <- R6::R6Class("syberia_engine",
  portable = TRUE,
  inherit = director:::director_, #environment(director::director)$director_,
  public = list(
    .parent  = NULL,
    .engines = list(),
    .set_parent = function(parent) { self$.parent <<- parent },

    register_engine = function(name, engine, mount = FALSE) {
      stopifnot(is(engine, "syberia_engine"))
      self$.engines[[name]] <<- list(engine = engine, mount = isTRUE(mount))
      if (isTRUE(mount)) engine$.set_parent(self)
    },

    resource = function(name, ..., parent. = TRUE, children. = TRUE,
                        exclude. = NULL, defining_environment. = parent.frame(),
                        engine) {
      if (!missing(engine)) {
        if (!is.simple_string(engine)) {
          stop("When sourcing a resource, please pass a string to the ",
               sQuote("engine"), " parameter.")
        }
        if (!is.element(engine, names(self$.engines))) {
          stop("There is no engine called ", sQuote(engine), ".")
        }
        engine_obj <- self$.engines[[engine]]$engine
        if (isTRUE(engine_obj$mount)) {
          stop("Explicit engine specification during resource sourcing is ",
               "only allowed on unmounted engines. The ", sQuote(engine), 
               " engine is a mounted engine.")
        }
        if (!engine_obj$exists(name, ..., parent. = FALSE)) {
          stop("No resource ", sQuote(name), " in engine ", sQuote(engine), ".")
        }
        return(engine_obj$resource(name, ..., parent. = FALSE))
      }

      ## Check the parent engines for resource existence.
      if (isTRUE(parent.) && !is.null(self$.parent)) {
        if (self$.parent$exists(name, parent. = TRUE, children. = TRUE, exclude. = list(self$root()))) {
          return(self$.parent$resource(name, exclude. = list(self$root()), ...,
                                       defining_environment. = defining_environment.))
        }
      }

      ## Check the current engines for resource existence.
      if (super$exists(name)) return(super$resource(name, ..., defining_environment. = defining_environment.))

      ## Check the subengines for resource existence.
      if (isTRUE(children.)) {
        for (engine in self$.engines) {
          if (isTRUE(engine$mount)) {
            engine <- engine$engine
            if (!any(vapply(exclude., should_exclude, logical(1), engine))) {
              if (engine$exists(name, parent. = FALSE, children. = TRUE, exclude. = exclude.)) {
                return(engine$resource(name, parent. = FALSE, children. = TRUE,
                                       exclude. = c(engine$root(), exclude.),
                                       ..., defining_environment. = defining_environment.))
              }
            }
          }
        }
      }

      ## Force trigger an error using the self director.
      super$resource(name, ..., defining_environment. = defining_environment.)
    },

    find = function(..., parent. = FALSE, children. = FALSE, exclude. = NULL,
                    check_duplicates. = FALSE, tag_engine. = FALSE) {
      ## Check the parent engines for resources.
      resources <- character(0)
      if (isTRUE(parent.) && !is.null(self$.parent)) {
        resources <- self$.parent$find(..., parent. = TRUE, children. = TRUE, exclude. = list(self$root()))
      }

      ## Check the current engines for resources.
      self_resources <- super$find(...)
      if (is.character(tag_engine.)) {
        names(self_resources) <- rep(tag_engine., length(self_resources))
      }
      resources <- c(self_resources, resources)
      resources <- resources[!duplicated(resources)]

      ## Check the subengines for resources.
      children_resources <- vector("list", length(self$.engines))
      if (isTRUE(children.)) {
        for (i in seq_along(self$.engines)) {
          name   <- names(self$.engines)[i]
          engine <- self$.engines[[i]]
          if (isTRUE(engine$mount)) {
            engine <- engine$engine
            if (!any(vapply(exclude., should_exclude, logical(1), engine))) {
              tag <- if (!identical(tag_engine., FALSE)) name else FALSE
              children_resources[[i]] <- engine$find(..., parent. = FALSE,
                children. = TRUE, exclude. = exclude., tag_engine. = tag)
              names(children_resources)[i] <- name
            }
          }
        }
      }

      if (isTRUE(check_duplicates.)) {
        children_resources <- Filter(Negate(is.null), children_resources)
        detect_duplicate_resources(children_resources)
      }
      resources <- c(recursive = TRUE, c(unname(children_resources), list(resources)))
      resources[!duplicated(resources)]
    },

    exists = function(resource, ..., parent. = TRUE, children. = TRUE, exclude. = NULL) {
      ## Check the parent engines for resource existence.
      if (isTRUE(parent.) && !is.null(self$.parent)) {
        if (self$.parent$exists(resource, ..., parent. = TRUE, children. = TRUE, exclude. = list(self$root()))) {
          return(TRUE)
        }
      }

      ## Check the current engines for resource existence.
      if (super$exists(resource, ...)) return(TRUE)

      ## Check the subengines for resource existence.
      if (isTRUE(children.)) {
        for (engine in self$.engines) {
          if (isTRUE(engine$mount)) {
            engine <- engine$engine
            if (!any(vapply(exclude., should_exclude, logical(1), engine))) {
              if (engine$exists(resource, ..., parent. = FALSE, children. = TRUE, exclude. = exclude.)) {
                return(TRUE)
              }
            }
          }
        }
      }

      FALSE
    }
  ),
  private = list(
  )
)

detect_duplicate_resources <- function(resource_list) {
  if (length(unique(c(recursive = TRUE, resource_list))) !=
      sum(vapply(resource_list, length, numeric(1)))) {
    # Allow for comparison of engine-resource pairs.
    engined_resource_list <- lapply(resource_list, function(resources) {
      setNames(Map(paste, names(resources), resources, sep = "\1"), resources)
    })

    pairs <- combn(length(resource_list), 2)
    conflict_list <- Filter(Negate(is.null), apply(pairs, 2, function(pair) {
      same_subengine <- engined_resource_list[[pair[1]]][
        engined_resource_list[[pair[1]]] %in% engined_resource_list[[pair[2]]]]
      common <- setdiff(intersect(resource_list[[pair[1]]], resource_list[[pair[2]]]),
                        names(same_subengine))
      if (length(common) > 0) {
        list(
          engines   = names(resource_list)[pair],
          resources = common
        )
      }
    }))

    if (length(conflict_list) > 0L) {
      conflicts <- do.call(paste, lapply(conflict_list, function(conflict) {
        paste0("\n\nEngine ", crayon::red(conflict$engines[1]), " and ",
               crayon::red(conflict$engines[2]), " share:\n",
          paste(collapse = "\n", paste(" *", vapply(conflict$resources, crayon::yellow, character(1)))))
      }))

      stop("Mounted child engines have conflicting resources. Please mount ",
           "at a different root or remove the conflicts: ", conflicts, "\n\n")
    }
  }
}


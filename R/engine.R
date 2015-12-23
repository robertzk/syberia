## Syberia is a framework that was crafted specifically for the R
## programming language. The story is similar to JavaScript: a single-purpose
## language meant to be used in a particular context exploded in popularity
## and found itself in a host of other settings.
##
## However, R is really just [LISP](https://en.wikipedia.org/wiki/Category:Lisp_programming_language_family)
## under the hood--a general purpose symbolic computing runtime. To prove that
## this makes R a viable production language for almost any purpose whatsoever,
## Syberia takes the first step of providing a hierarchical structure for R
## projects. Anything--from an empty directory, to an R package, to a collection
## of shiny dashboards--can be a Syberia project.
##
## This flexibility is achieved by assuming nothing about the underlying
## directory structure: in other words, fixing no conventions, but letting
## the user decide what the right course of action should be with respect
## to structuring their project. Other tools fare poorly at this; for example,
## a package does not even let you have a hierarchical directory structure
## in its `R/` subdirectory; what an affront to developer sanity!
##
## How can Syberia claim to be a convention-over-configuration framework
## if it fixes no conventions? The answer is that while Syberia *itself*,
## the package you are currently examining, does not fix any conventions,
## the de facto approach to development within Syberia is as follows:
##
## 1. [Pick an engine to build your work off of](http://github.com/syberia/modeling.sy).
## 2. [Build your project](https://github.com/syberia/examples) within the conventions
##    set by your choice of engine.
##
## It is possible to include multiple engines, or a whole complex potentially
## cyclic graph of engines, but this introduces [the diamond problem](https://en.wikipedia.org/wiki/Multiple_inheritance)
## and should generally be avoided until you have a good understanding of
## computer science foundations.
##
## In other words, the Syberia approach is to be a *meta-framework*. By making
## no assumptions about the structure of your project, it is your choice which
## engine to build your project on depending on what suits your needs. This is
## especially helpful for machine learning projects, where the task and solution
## can take widely varying shapes depending on whether the problem is supervised,
## unsupervised, NLP, deep learning, etc.
## 
## The above 2-step approach is recursive. For example, the [modeling engine](https://github.com/syberia/modeling.sy),
## the default engine for most projects, is built off the [base engine](https://github.com/syberia/base.sy),
## which dictates that each project should have a `config/routes.R` file which
## links the `lib/controllers` directory to the rest of the project and tells 
## you how R scripts in the project are to be parsed according to which
## directory they reside in. This is similar to object-oriented programming,
## except that it is strictly more general since it does not force you 
## to treat every single thing in the world as an object.
##
## Thus, the core structural unit is an **engine**. In order to bootstrap
## effectively, an engine should have a `config/application.R` file, potentially
## empty, so that Syberia can detect this is an engine, as well as a `config/engines.R`
## file indicating which engine this one depends on, if any. This is the sole
## convention and can be pretty easily deprecated in future versions of Syberia
## if there is demand for more flexible configuration.
##
## Each engine is **testable** by design. Syberia exports a function called
## `test_project` that by default looks in the `test/` directory of the
## engine and requires all resources have an accompanying test. If your
## resource (e.g., R script) does not have a test -- it fails! In general,
## only well-written code is easily testable so this encourages both
## separating your project out into similar components through like
## directory structures as well as ensuring all inputs and outputs are
## what you expect them to be. (Not to mention saving you a huge headache
## of managing the complexity of a growing system!)
##
## To construct a Syberia engine object, run `syberia_project("/path/to/engine")`.
## This is a `syberia_engine` [R6 class](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)
## and holds together everything that Syberia knows about your project.
## It has methods like `$find`, `$resource`, and `$exists` to play with
## the files in your project. In general, Syberia takes a page out of
## [node.js](https://nodejs.org/en/)'s book and encourages all files
## to be structured so they have a single *export*: the last expression
## in the file. If you haven't replaced the default controller
## using the `config/routes` file, a topic we'll touch on later, you
## should be able to "compile" your resource using
##
## ```r
## value <- project$resource("relative/path/to/resource.R")
## ```
##
## For example, if we had
##
## ```r
## # relative/path/to/resource.R
## x <- 1
## y <- 2
## x + y
## ```
##
## then `value` above would be `3`. However, as we will see,
## the `project$resource` caller is capable of much more than
## [sourcing a file](http://www.inside-r.org/r-doc/base/source).
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
#'    By default, the current directory.
#' @param ... Additional arguments used internally.
#' @param root. logical. Whether or not this is a root-level engine,
#'    by default \code{TRUE}.
#' @export
#' @note The syberia package will maintain an internal cache of engines.
#'    Therefore, calling \code{syberia_engine} twice will retrieve the
#'    cached object. This cache is maintained in the \code{.syberia_env}
#'    environment object in the syberia package namespace.
#' @return The \code{\link[director]{director}} object responsible for
#'    managing the engine.
syberia_engine <- function(filepath = getwd(), ..., root. = TRUE) {
  project <- syberia_engine_(filepath, ...)
  if (isTRUE(root.)) {
    .syberia_env$active_project <- project
  }
  project
}

#' @rdname syberia_engine
#' @export
syberia_project <- syberia_engine

#' The current active Syberia project.
#'
#' @return A \code{syberia_engine} object or \code{NULL}.
#' @export
active_project <- function() {
  .syberia_env$active_project
}

#' @rdname active_project
#' @name project
#' @export
makeActiveBinding("project", function() active_project(), env = environment())

#' Compile a resource in the active syberia project.
#'
#' @seealso \code{\link{active_project}}
#' @name resource
#' @export
makeActiveBinding("resource", function() active_project()$resource, env = environment())

syberia_engine_ <- function(filepath, ...) {
  UseMethod("syberia_engine_", filepath)
}

syberia_engine_.pre_engine <- function(filepath, ...) {
  build_engine(filepath)
}

syberia_engine_.character <- function(filepath, ...) {
  syberia_engine_character(filepath, ...)
}

syberia_engine_character <- function(filepath, cache = TRUE) {
  ## If a user gives `~/foo/bar/baz` as the path and the project's
  ## root is in fact `~/foo` (in other words, if they give a file or subdirectory 
  ## in the project), this should be inferrable. We traverse
  ## the parent directories until we hit the root of the file system
  ## to see if we are in a syberia engine
  traverse_parent_directories(normalizePath(filepath, mustWork = FALSE), function(filepath) {
    ## If we are caching the precomputed `syberia_engine` object, simply fetch
    ## it from the `.syberia_env` helper environment.                              
    if (isTRUE(cache) && has_application_file(filepath)) {
      ## If it is not cached, call `build_engine` on the directory.
      .syberia_env[[filepath]] <- .syberia_env[[filepath]] %||% build_engine(filepath)
    } else if (has_application_file(filepath)) {
      build_engine(filepath)
    }
  ## If we did not find a syberia engine, we just crash here.
  }, error = sprintf("No syberia engine found at %s", sQuote(crayon::red(filepath))))
}

## A syberia engine with root `root` is, by definition, a directory of
## R files containing the file `root/config/application.R` or any variation
## thereof (e.g., `root/config/application/application.r`). This is how we
## recognize the engine.
extensions <- c(".R", ".r", "/application.R", "/application.r")
has_application_file <- function(filepath) {
  any(file.exists(paste0(file.path(filepath, "config", "application"), extensions)))
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
  syberia_engine(dir, cache = FALSE, root. = FALSE)
}

#' @export
build_engine.character <- function(buildable) {
  # bootstrap_engine(director::director(buildable))
  bootstrap_engine(syberia_engine_class$new(buildable))
}

engine_dir <- function(dir) {
  file.path(engine_location(), dir)
}

## The location where Syberia will keep copies of downloaded engines.
engine_location <- function() {
  path <- engine_location_path()
  if (!file.exists(path)) {
    ## Ensure we can actually create the engine storage location.
    if (!dir.create(path, FALSE, TRUE)) {
      stop(m("engine_location_mismatch", path = path), call. = FALSE)
    }
  }
  path
}

## By default, Syberia will store engine code in
## `~/.R/.syberia/engines`.
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
                 cache = FALSE, root. = FALSE)
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


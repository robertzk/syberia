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
## recognize the engine. (Alternatively, if it has a `config/engines` resource.)
has_application_file <- function(filepath) {
  extensions <- c(".R", ".r", "/_.R", "/_.r")
  files <- function(...) {
    c(recursive = TRUE, lapply(list(...), function(type) {
      paste0(file.path(filepath, "config", type),
             gsub(fixed = TRUE, "_", type, extensions))
    }))
  }
  any(file.exists(files("application", "engines")))
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
  ## To build an engine, we bootstrap an otherwise bare
  ## `syberia_engine` R6 object. Bootstrapping an engine
  ## is explained below.
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

## To build an engine, we bootstrap an otherwise bare
## `syberia_engine` R6 object. Bootstrapping an engine
## consists of
##
## * Registering preprocessors for `config/boot` and `config/engines`.
## * Registering a parser for `config/engines`.
## * Executing `config/engines` followed by `config/boot`.
## * Ensure that the ensure has no conflicting resources with any other engine.
## * Set the cache entry `bootstrapped` on the engine to indicate it is built.
##
## The notion of preprocessor and parser belongs to
## [director](https://github.com/syberia/director). In fact, every `syberia_engine`
## inherits from `director`, so every `syberia_engine` has at least the same
## capabilities as a `director` object. (Rails experts may notice the similarity
## between Syberia engines and directors, and Rails engines and railties.)
##
## Registering a preprocessor means we do *additional stuff* before sourcing
## that file. For example, in `config/engines`, we provide a helper function
## called `engine` for registering subengines. Registering a parser means we
## do additional stuff *after* preprocessing and sourcing that file. For example,
## in `config/engines` we need to actually build and mount the engines referred
## to in the fil with the `engine` helper function.
##
## In order to avoid the [diamond problem](https://en.wikipedia.org/wiki/Multiple_inheritance),
## Syberia ensures that engines do not share resources *unless* they come from
## a common base engine. This is a technical issue that will eventually be
## explained in a more thorough whitepaper. (For the confused CS students asking
## why I couldn't have simply made engines form a [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
## instead of a possibly cyclic ## graph--the short answer is that engines need to
## trivially modularizable while ## retaining the power to refer to parent engine
## resources dynamically and just-in-time. I may very well regret this decision.)
##
## Each `director`, and by proxy `syberia_engine`, has an internal cache for
## memoising run-time state while the R process is running (this is different
## from a `director`'s *registry*, which captures persistent cross-session
## state on disk). We simply set the `bootstrapped` key to `TRUE` so we can
## check for it later.
bootstrap_engine <- function(engine) {
  if (isTRUE(engine$cache_get("bootstrapped"))) return(engine)
  engine$register_preprocessor("config/boot",    boot_preprocessor)
  engine$register_preprocessor("config/engines", engine_preprocessor)
  engine$register_parser      ("config/engines", engine_parser)

  exists <- function(...) engine$exists(..., parent. = FALSE, children. = FALSE)
  if (exists("config/engines")) engine$resource("config/engines")
  if (exists("config/boot"))    engine$resource("config/boot")

  # Check for duplicate resources in mounted child engines.
  engine$find(check_duplicates. = TRUE, children. = TRUE, tag_engine. = TRUE)
  engine$cache_set("bootstrapped", TRUE)
  engine
}

## The `boot_preprocessor` is referred to above in the `boostrap_engine` function.
## We are saying that the `config/boot` file should have access to
## the `director` object, and nothing more.
boot_preprocessor <- function(source, source_env, director) {
  source_env$director <- director
  source()
}

## The `config/engines` file is an epsilon harder. Basically, we use
## the `engine` helper function to record the user's requested engine
## mounting until we get to the parser below.
engine_preprocessor <- function(source, source_env, preprocessor_output, director) {
  if (isTRUE(director$cache_get("bootstrapped"))) return()

  preprocessor_output$engines <- new.env(parent = emptyenv())
  source_env$engine <- function(name, ...) {
    preprocessor_output$engines[[name]] <- list(...)
  }
  source()
}

## Now that we have collected the engines to be mounted into the `preprocessor_output`
## helper (which also came from [director](https://github.com/syberia/director)),
#
engine_parser <- function(director, preprocessor_output) {
  if (isTRUE(director$cache_get("bootstrapped"))) return()

  ## For each engine mentioned using the `engine` helper in `config/engines`,
  ## we *register* the engine. This means that we establish member variables
  ## on the respective `syberia_engine` objects that allow them to understand
  ## the topology: the parent engine knows about its children, and each child
  ## knows about its parent. Syberia is all about family.
  for (engine in ls(preprocessor_output$engines, all = TRUE)) {
    register_engine(director, engine, parse_engine(preprocessor_output$engines[[engine]]),
                    mount = isTRUE(preprocessor_output$engines[[engine]]$mount))
  }

  ## When we use `devtools::load_all` on director, it loads a symbol called
  ## `exists`; we use explicit base namespacing to prevent conflicts during development.
  if (base::exists(".onAttach", envir = input, inherits = FALSE)) {
    ## A little additional trick is that the user could have specified
    ## an `.onAttach` function in the `config/engines` file. If this exists,
    ## we store it in the `syberia_engine` cache and later invoke it.
    onAttach <- input$.onAttach

    if (!is.function(onAttach)) {
      stop(m("onattach_failure", root = director$root(), klass = class(onAttach)[1L]),
           call. = FALSE)
    }

    ## We use `list2env` to "inject" the `director` local variable into the
    ## scope of the `onAttach` hook.
    environment(onAttach) <- list2env(
      list(director = director),
      parent = environment(onAttach) %||% baseenv()
    )
    director$cache_set(".onAttach", onAttach)
  }
  NULL
}

## Registering an engine means making the parent aware of its child
## and the child aware of its parent. Mounting the engine means
## we will be treating a collection of engines, each in potentially
## very different directories on the file system, as *one giant project*.
## This allows us to pull out a subset of Syberia resources and
## "enginify" them with ease.
##
## Although Syberia does face a few [theoretical challenges](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem),
## these have solutions, and the end result is that given a Syberia
## project with `N` resources, there are `2^N` possible engines extractable
## from that project: one for *each subset of resources*. One of Syberia's goals
## is to make it as easy as possible to pull out your work so others
## can re-use it.
register_engine <- function(director, name, engine, mount = FALSE) {
  message(crayon::green(paste("...Mounting", name, "engine.")))

  # TODO: (RK) Replace with $engines private member after R6ing.
  if (!director$cache_exists("engines")) {
    director$cache_set("engines", new.env(parent = emptyenv()))
  }
  env <- director$cache_get("engines")
  env[[name]] <- engine

  director$register_engine(name, engine, mount = mount)

  if (engine$cache_exists(".onAttach")) {
    ## This is where we invoke the `.onAttach` hook registered in
    ## the `engine_parser`.
    engine$cache_get(".onAttach")(director)
  }
}

parse_engine <- function(engine_parameters) {
  ## By default, download engines from GitHub.
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
  ## GitHub-derived engines need to provide a `repo` and `version` (by default "master").
  ## For example, the following indicates we wish to load the "bobbleheads"
  ## engine from GitHub "jimbob/bobbleheads.sy" off the master branch.
  ##
  ## ```r
  ## # config/engines.R
  ## engine("bobbleheads", type = "github", repo = "jimbob/bobbleheads.sy")
  ## ```
  ##
  ## Syberia will download and cache the engine.
  repo    <- engine_parameters$repo %||% engine_parameters$repository
  # TODO: (RK) Checking for updates?
  version <- engine_parameters$version %||% "master"
  stopifnot(is.simple_string(repo))

  pre_engine(prefix = file.path("github", repo, version),
    builder = function(filepath) {
      status <- system2("git", c("clone", sprintf("https://github.com/%s.git", repo), filepath))
      stopifnot(status == 0)
    })
}

## Putting the following in `config/engines.R` indicates we wish
## to load the "bobbleheads" engine from GitHub
## "jimbob/bobbleheads.sy" off the master branch.
##
## ```r
## # config/engines.R
## engine("bobbleheads", type = "local", path = "~/dev/bobbleheads")
## ```
parse_engine.local <- function(engine_parameters) {
  path <- engine_parameters$path %||% stop("Please provide an engine path")
  if (!file.exists(path)) stop("The path ", sQuote(path), " does not exist.")
  path
}

pre_engine <- function(prefix, builder) {
  ## Build a `pre_engine` S3 object.
  structure(list(prefix = prefix, builder = builder), class = "pre_engine")
}

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

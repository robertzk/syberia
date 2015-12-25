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

      if (missing(engine)) {
        # If we do not force right away, parent.frame() will be pointing to
        # the wrong place. Everything in R is a promise...
        force(defining_environment.)
        dots <- list(...)
        private$traverse_tree(parent = parent., children = children.,
          exclude = exclude., exists_args = list(name),
          on_parent = self$.parent$resource(name, ...,
                        exclude. = c(list(self$root()), exclude.),
                        defining_environment. = defining_environment.),
          on_self = super$resource(name, ...,
                                   defining_environment. = defining_environment.),
          on_child = function(engine) {
            do.call(engine$resource, c(list(name), dots, list(
              parent. = FALSE, children. = TRUE,
              exclude. = c(engine$root(), exclude.),
              defining_environment. = defining_environment.)
            ))
          },
          otherwise = super$resource(name, ...,
                                     defining_environment. = defining_environment.)
        )
      } else {
        stopifnot(is.character(engine))
        engine_name <- engine
        engine <- private$sanitize_engine(engine)
        if (!engine$exists(name, ..., parent. = FALSE)) {
          stop("No resource ", sQuote(name), " exists in engine ", sQuote(engine_name), ".")
        }
        return(engine$resource(name, ..., parent. = FALSE))
      }
    },

    find = function(..., parent. = FALSE, children. = FALSE, exclude. = NULL,
                    check_duplicates. = FALSE, tag_engine. = FALSE) {
      ## Check the parent engines for resources.
      resources <- character(0)
      if (isTRUE(parent.) && private$has_parent()) {
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
      private$traverse_tree(parent = parent., children = children.,
        exclude = exclude., exists_args = list(resource, ...),
        on_parent = TRUE, on_self = TRUE,
        on_child = function(engine) TRUE, otherwise = FALSE)
    }
  ),
  private = list(
    has_parent = (has_parent <- function() { !is.null(self$.parent) }),
    is_root    = Negate(has_parent),
    mounted_engines = function() {
      Filter(function(e) isTRUE(e$mount), self$.engines)
    },

    sanitize_engine = function(engine) {
      if (!is.simple_string(engine)) {
        stop(m("sanitize_engine_class"), call. = FALSE)
      }
      if (!is.element(engine, names(self$.engines))) {
        stop(m("sanitize_engine_no_engine", engine = engine), call. = FALSE)
      }
      engine_obj <- self$.engines[[engine]]$engine
      if (isTRUE(engine_obj$mount)) {
        stop(m("sanitize_engine_mounting_conflict", engine = engine), call. = FALSE)
      }
      engine_obj
    },

    traverse_tree = function(parent, children, on_parent, on_self, on_child, otherwise,
                             exists_args, exclude) {

      full_exists_args <- c(exists_args, exclude. = c(list(self$root())),
        parent. = TRUE, children. = TRUE)

      if (isTRUE(parent) && private$has_parent()) {
        if (do.call(self$.parent$exists, full_exists_args)) {
          return(eval.parent(substitute(on_parent)))
        }
      }

      if (do.call(super$exists, exists_args)) {
        return(eval.parent(substitute(on_self)))
      }
      
      if (isTRUE(children)) {
        for (engine in private$mounted_engines()) {
          engine <- engine$engine
          if (!any(vapply(exclude, should_exclude, logical(1), engine))) {
            full_exists_args$parent. <- FALSE
            if (do.call(engine$exists, full_exists_args)) {
              return(on_child(engine))
            }
          }
        }
      }

      eval.parent(substitute(otherwise))
    }
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


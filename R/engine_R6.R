## The core object coordinating a syberia engine is a
## `syberia_engine` [R6](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)
## object.
syberia_engine_class <- R6::R6Class("syberia_engine",
  portable = TRUE,
  ## We inherit directly from the `director` base class.
  inherit = getFromNamespace("director_", "director"), #environment(director::director)$director_,
  public = list(
    ## Each syberia engine can include more dependent engines.
    ## This forms a graph of engines. However, child engines are allowed
    ## to be shared, so the final structure is usually a DAG, not a tree.
    ## The parent node is stored in `.parent` and the children
    ## (immediate dependent engines) are stored in `.engines`.              
    .parent  = NULL,
    .engines = list(),
    .set_parent = function(parent) { self$.parent <<- parent },

    ## Registering an engine is as simple adding it to the list of
    ## `.engines`. If we are mounting it, we register its parent
    ## node as the current engine.
    register_engine = function(name, engine, mount = FALSE) {
      stopifnot(is(engine, "syberia_engine"))
      self$.engines[[name]] <<- list(engine = engine, mount = isTRUE(mount))
      if (isTRUE(mount)) engine$.set_parent(self)
    },

    ## The vanilla `director` object that the `syberia_engine` inherits
    ## from has a `resource` method: namely, take an R file and compile it
    ## according to some preprocessor and parser (see the director package
    ## for a more thorough explanation).
    ## 
    ## The `syberia_engine$resource` method takes a different approach.
    ## Since an engine may itself have more engines, resources can come
    ## from a whole tree of engines. Typically, it would be sufficient
    ## to check the engine itself and its children when looking for a
    ## resource. However, Syberia takes a different approach. To make
    ## it possible to pull out arbitrary subsets of files and turn them
    ## into an engine, Syberia first traverses *up* to the parent to
    ## see if it has overwritten any of the resources.
    ##
    ## This leads to some multiple inheritance problems and is a tricky
    ## course to navigate, but the end result is that the user is completely
    ## transparent to the machinery that goes into finding resources
    ## under the hood. It is possible to extract a collection of controllers
    ## and resources from any top-level syberia project (as long as they are
    ## self contained and include each other's dependencies) and transform
    ## that into an engine simply by copying the files, while retaining
    ## the power provided by controllers insofar as giving each directory
    ## structure its own meaning.
    resource = function(name, ..., parent. = TRUE, children. = TRUE,
                        exclude. = NULL, defining_environment. = parent.frame(),
                        engine) {

      if (missing(engine)) {
        # If we do not force right away, parent.frame() will be pointing to
        # the wrong place. Everything in R is a promise...
        force(defining_environment.)
        dots <- list(...)
        ## We use a tree traversal helper defined later in this file:
        ## first, it checks the parent exists (if one exists). This
        ## will recursively call its parent's `resource` method. To avoid
        ## infinite loops, we have to be careful about excluding the
        ## current engine from traversal. If the parent engine reports
        ## it owns the resource, we return the `on_parent` action.
        ## 
        ## Otherwise, if the current engine has the resource, we return
        ## that instead. Finally, if neither the parent engine nor self
        ## own the resource, we browse through the children.
        ##
        ## In the event that none of the engines have the resource,
        ## we trigger the self engine to resource the object so
        ## that we get an error that the resource was not found.
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
      } else { # if (!missing(engine))
        ## It is possible to "pluck" a resource explicitly out of a utility engine
        ## using the `engine` parameter (if the engine is not mounted).
        stopifnot(is.character(engine))
        engine_name <- engine
        engine <- private$sanitize_engine(engine)
        if (!engine$exists(name, ..., parent. = FALSE)) {
          stop("No resource ", sQuote(name), " exists in engine ", sQuote(engine_name), ".")
        }
        ## Extract the resource directly from the utility engine.
        return(engine$resource(name, ..., parent. = FALSE))
      }
    },

    ## Finding resources (as opposed to compiling them) follows a similar
    ## pattern: we first check the parent, then self, then the children.
    ## However, unlike `$resource`, the `$find` method must traverse
    ## all of them and take their union, similarly to the difference
    ## between calling [`Reduce`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/funprog.html)
    ## with and without the `accumulate` parameter. In some sense, 
    ## the find operation is a tree traversal pattern with the action of
    ## "find the resources matching these arguments" and a final
    ## reduce step.
    find = function(..., parent. = FALSE, children. = FALSE, exclude. = NULL,
                    check_duplicates. = FALSE, tag_engine. = FALSE) {

      resources <- private$traverse_tree(parent = parent., children = children.,
        exclude = exclude., accumulate = TRUE,
        on_parent = self$.parent$find(..., parent. = TRUE, children. = TRUE,
          exclude. = c(list(self$root()), exclude.), tag_engine. = tag_engine.),
        on_self = {
          ## This is a bit of a subtle point. We tag the resources with the
          ## name of the engine so that we can later avoid some problems related
          ## to [multiple inheritance](https://en.wikipedia.org/wiki/Multiple_inheritance)
          self_resources <- super$find(...)
          if (is.character(tag_engine.)) {
            `names<-`(self_resources, rep(tag_engine., length(self_resources)))
          } else {
            self_resources
          }
        },
        on_child = function(engine, name) {
          tag <- if (!identical(tag_engine., FALSE)) name else FALSE
          engine$find(..., parent. = FALSE,
            children. = TRUE, exclude. = exclude., tag_engine. = tag)
        }
      )

      if (isTRUE(children.)) {
        if (isTRUE(check_duplicates.)) {
          ## If we are checking child engines, we take this opportunity to
          ## ensure none of the mounted engines have *shared* resources.
          ## For example, if two engines both have a `foo/bar.R` file, this
          ## would cause terrible conflicts related to multiple inheritance:
          ## if the engine1 asked for `foo/bar.R`, then since the tree  
          ## traversal asks the *parent* firsts, which asks its non-engine1
          ## children, then engine1 would receive `foo/bar.R` from engine2.
          ## Conversely, engine2 would receive engine1's `foo/bar.R`, probably
          ## leading to all sorts of perverse bugs. In the future it may
          ## be possible to specify canonical preferences in configuration
          ## but for now we disable shared resources all together:
          ##
          ## **All engines must be disjoint.**
          ##
          ## (Except insofar as they share resources from a common child
          ## engine, as in a diamond graph.)
          resources[[length(resources)]] <- Filter(Negate(is.null), resources[[length(resources)]])
          detect_duplicate_resources(resources[[length(resources)]])
        }
        resources[[length(resources)]] <- unname(resources[[length(resources)]])
      }

      ## We revert the list to ensure children resources mask self
      ## and parent resources (or, for example, we would get a conflict
      ## in a simple diamond with two engines depending on a base
      ## engine, which has `config/application.R` and `config/engines.R`).
      resources <- c(recursive = TRUE, rev(resources))
      ## Using `unique` here would drop names!
      resources[!duplicated(resources)]
    },

    exists = function(resource, ..., parent. = TRUE, children. = TRUE, exclude. = NULL) {
      ## Check if a resource exists is a straightforward tree traversal
      ## pattern: check the parent, then self, then the children,
      ## and return `TRUE` as soon as any find them. Otherwise, return `FALSE`.
      private$traverse_tree(parent = parent., children = children.,
        exclude = exclude., exists_args = list(resource, ...),
        on_parent = TRUE, on_self = TRUE,
        on_child = function(engine) TRUE, otherwise = FALSE)
    }
  ),

  ## This forms a collection of private helper methods.
  private = list(
    has_parent = (has_parent <- function() { !is.null(self$.parent) }),
    is_root    = Negate(has_parent),
    mounted_engines = function() {
      Filter(function(e) isTRUE(e$mount), self$.engines)
    },

    ## We convert an engine from string representation to a
    ## `syberia_engine` object.
    sanitize_engine = function(engine, utility = TRUE) {
      if (!is.simple_string(engine)) {
        stop(m("sanitize_engine_class"), call. = FALSE)
      }
      if (!is.element(engine, names(self$.engines))) {
        stop(m("sanitize_engine_no_engine", engine = engine), call. = FALSE)
      }
      engine_obj <- self$.engines[[engine]]$engine
      if (isTRUE(utility) && isTRUE(engine_obj$mount)) {
        stop(m("sanitize_engine_mounting_conflict", engine = engine), call. = FALSE)
      }
      engine_obj
    },

    ## The master helper that defines `$find`, `$resource`, and `$exists`.
    ## A little many arguments for my usual taste, but they make sense here:
    ## we specify whether we would like to operate on the `parent` or `children`
    ## first as scalar bools, followed by expressions for `on_parent` and `on_self`
    ## and a function for `on_child`. We use a function for the latter since it
    ## may operate on multiple children whereas the parent and self are singletons.
    ##
    ## If the `on_child` function has arity 1 it receives the `syberia_engine`
    ## object as its argument. On arity 2, it also receives the name.
    ##
    ## Finally, we can specify which arguments to pass to `director$exists`
    ## when checking for resource existence as well as which engines to exclude.
    ## If this is missing, resources will not be checked for existence prior
    ## to running parent, self, or child actions.
    ## 
    ## Finally, if `accumulate = TRUE`, we return a list of the three results
    ## (parent, self, children) with a list of multiple results per child as
    ## the third element.
    traverse_tree = function(parent, children, on_parent, on_self, on_child, otherwise,
                             exists_args, exclude, accumulate = FALSE) {

      record <- if (isTRUE(accumulate)) {
        result <- list()
        function(value, subindex) {
          if (missing(subindex)) {
            result[[length(result) + 1]] <<- value
          } else {
            result[[length(result)]][[subindex]] <<- value
          }
        }
      } else { `return` }

      check_exists <- !missing(exists_args)

      if (check_exists) {
        full_exists_args <- c(exists_args, exclude. = c(list(self$root())),
          parent. = TRUE, children. = TRUE)
      }

      if (isTRUE(parent) && private$has_parent()) {
        ## A little tricky here: If we do not check for existence, always
        ## run the `on_parent` action, otherwise only check it if the parent
        ## engine has the resource.
        if (!check_exists || do.call(self$.parent$exists, full_exists_args)) {
          record(eval.parent(substitute(on_parent)))
        }
      }

      ## Similarly, if we do not check for existence, always
      ## run the `on_self` action, otherwise only check it if the current 
      ## engine has the resource.
      if (!check_exists || do.call(super$exists, exists_args)) {
        record(eval.parent(substitute(on_self)))
      }
      
      if (isTRUE(children)) {
        ## The third element returned by the tree traversal when
        ## `accumulate = TRUE` will be a list of children.
        if (isTRUE(accumulate) && length(formals(on_child)) > 1L) {
          record(list())
        }
        if (check_exists) {
          ## No longer ask the parent during existence checks
          ## (or we would have infinite loops!).
          full_exists_args$parent. <- FALSE
        }
        engines <- private$mounted_engines()
        for (i in seq_along(engines)) {
          name   <- names(engines)[i]
          engine <- engines[[i]]$engine

          if (!any(vapply(exclude, should_exclude, logical(1), engine))) {
            if (!check_exists || do.call(engine$exists, full_exists_args)) {
              if (length(formals(on_child)) == 1L) {
                record(on_child(engine))
              } else {
                record(on_child(engine, name), name)
              }
            }
          }
        }
      }

      if (isTRUE(accumulate)) {
        result
      } else {
        ## Note that if `accumulate = FALSE`, we should never get
        ## here if `on_parent`, `on_self`, or `on_child` had a return
        ## action.
        eval.parent(substitute(otherwise))
      }
    }
  )
)

## A rather technical helper function to ensure that child engines
## of a common parent engines do not share resources.
##
## For example, if two engines both have a `foo/bar.R` file, this
## would cause terrible conflicts related to multiple inheritance:
## if the engine1 asked for `foo/bar.R`, then since the tree  
## traversal asks the *parent* firsts, which asks its non-engine1
## children, then engine1 would receive `foo/bar.R` from engine2.
## Conversely, engine2 would receive engine1's `foo/bar.R`, probably
## leading to all sorts of perverse bugs. In the future it may
## be possible to specify canonical preferences in configuration
## but for now we disable shared resources all together:
##
## **All engines must be disjoint.**
##
## (Except insofar as they share resources from a common child
## engine, as in a diamond graph.)
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
          paste(collapse = "\n", paste(" *",
            vapply(conflict$resources, crayon::yellow, character(1)))))
      }))

      stop("Mounted child engines have conflicting resources. Please mount ",
           "at a different root or remove the conflicts: ", conflicts, "\n\n")
    }
  }
}


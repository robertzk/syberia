#' Syberia provides an opinionated unified framework for
#' fast iteration on classifier development and deployment. It is
#' founded on convention over configuration and aims to solve the
#' problems of classifier-specific data preparation and
#' classifier-specific modeling parameters.
#'
#' @name syberia
#' @docType package
#' @import testthat devtools crayon stringr memoise Matrix Ramd stagerunner statsUtils mungebits tundra syberiaMungebits syberiaStructure syberiaStages director objectdiff R6
NULL

## An environment used for caching some syberia-managed objects, like directors.
.syberia_env <- new.env(parent = emptyenv())

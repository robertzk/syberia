context('construct_stage_runner')
require(testthatsomemore)

test_that('it can accept stages with arity 1', {
  construct_stage_runner <- force(construct_stage_runner)
  stub(construct_stage_runner, .example_stage) <- 
    function(modelenv) function(modelenv) modelenv$x <- 1
  sr <- construct_stage_runner(list(.example = list()))
  rm('.example_stage', envir = environment(construct_stage_runner))
  sr$run()
  expect_identical(sr$context$x, 1)
})

test_that('it can accept stages with arity 1 taking just options', {
  construct_stage_runner <- force(construct_stage_runner)
  stub(construct_stage_runner, .example_stage) <- 
    function(options) { force(options); function(modelenv) modelenv$x <- options$x }
  sr <- construct_stage_runner(list(.example = list(x = 1)))
  rm('.example_stage', envir = environment(construct_stage_runner))
  sr$run()
  expect_identical(sr$context$x, 1)
})

test_that('the argument "parameters" is equivalent to options', {
  construct_stage_runner <- force(construct_stage_runner)
  stub(construct_stage_runner, .example_stage) <- 
    function(params) { force(params); function(modelenv) modelenv$x <- params$x }
  sr <- construct_stage_runner(list(.example = list(x = 1)))
  rm('.example_stage', envir = environment(construct_stage_runner))
  sr$run()
  expect_identical(sr$context$x, 1)
})


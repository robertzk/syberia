context("syberia projects")
library(testthatsomemore)

describe("Project example1", {
  test_that("it can load the syberia engine representing example1", {
    assert(syberia_engine("projects/engines1/main"))
  })

  test_that("it can load a primary resource", {
    expect_equal(syberia_engine("projects/engines1/main")$resource("main_resource"),
                 "main_resource")
  })

  test_that("it can load a secondary resource", {
    expect_equal(syberia_engine("projects/engines1/main")$resource("example_resource"),
                 "example_resource")
  })
})

describe("Project example2", {
  test_that("it can overwrite an earlier resource", {
    expect_equal(syberia_engine("projects/engines2/main")$resource("example_resource"),
                 "overwritten")
  })
})

describe("Project example_controller", {
  test_that("it can apply a simple controller with the base engine", {
    expect_equal(syberia_engine("projects/example_controller")$resource("example"),
                 list(first = 1, second = 2))
  })

  test_that("it can apply a simple controller with a preprocessor", {
    expect_equal(syberia_engine("projects/example_controller")$resource("simple_preprocessor"),
                 list(first = 1, second = 2))
  })

  test_that("it has access to the director object", {
    dir <- file.path(getwd(), "projects", "example_controller")
    expect_equal(syberia_engine("projects/example_controller")$resource("director"), dir)
  })

  test_that("it has access to the director object", {
    # TODO: (RK) This will break if we pass o = 1 instead of a = 1
    # See line 25 of resource-parser.R in director. We need to *capture*
    # the list(...) as early as possible and pass it as an argument along
    # the resource tower or we will run into partial argument matching
    # and have some pretty weird heisenbugs if there is a collision with
    # any of the arguments in the tower chain.
    expect_equal(syberia_engine("projects/example_controller")$resource("args", a = 1, b = 2, d = 3),
                 list(a = 1, b = 2, d = 3))
  })

  test_that("it has access to the sourcing environment", {
    expect_equal(syberia_engine("projects/example_controller")$resource("source_env"),
                 list(a = 1))
  })

  test_that("it has access to the resource and file name", {
    file <- normalizePath(file.path(getwd(), "projects", "example_controller", "names.R"))
    expect_equal(syberia_engine("projects/example_controller")$resource("names"),
                 list(resource_name = "names", filename = file, prefilename = file))
  })

  test_that("it has access to the preprocessor_output", {
    expect_equal(syberia_engine("projects/example_controller")$resource("preprocessor_output"),
                 list(hello = "world"))
  })

  # TODO: (RK) Add tests for dependency modification.
})

describe("Project two_engines", {
  test_that("it can simultaneously use two engines", {
    assert(syberia_engine("projects/two_engines/main"))
  })

  test_that("it can fetch a resource from one engine", {
    expect_equal(syberia_engine("projects/two_engines/main")$resource("example_resource"),
                 "example_resource1")
  })
})

describe("Project conflicting_resources", {
  test_that("it can detect resource conflicts in mounted engines", {
    expect_error(syberia_engine("projects/conflicting_resources/main"),
                 "conflicting resources")
                
  })
})

describe("Project snake", {
  test_that("it can follow an engine chain", {
    expect_equal(syberia_engine("projects/snake/main")$resource("engine_resource"),
                 "down in the deep")
  })
})

describe("Project double_snake", {
  test_that("it can follow parallel engine chains", {
    expect_equal(syberia_engine("projects/double_snake/main")$resource("engine_resource"),
                 "down in the deep")
    expect_equal(syberia_engine("projects/double_snake/main")$resource("addon_resource"),
                 "welcome to the jungle")
  })
})

describe("Project conflicting_double_snake", {
  test_that("it can detect resource conflicts through chains of engines", {
    expect_error(syberia_engine("projects/conflicting_double_snake/main"),
                 "conflicting resources")
    expect_error(syberia_engine("projects/conflicting_double_snake/main"),
                 "engine_resource")
                
  })
})

describe("Project diamond_with_a_tail", {
  test_that("it can follow a diamond down to a base through a parent of base", {
    expect_equal(syberia_engine("projects/diamond_with_a_tail/main")$resource("common_resource"),
                 "we share so much in common")
  })
})


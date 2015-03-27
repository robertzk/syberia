# Only run during non-interactive mode
#if (!interactive()) {
#  context("Loading github packages")
#
#  test_that("attempting to load a non-existent Github package fails", {
#    expect_error(
#      load_github_packages(list('nonexistent_package', 'nonexistent_author')),
#      "client error")
#  })
#
#  test_that("attempting to load an existent Github package succeeds", {
#    expect_error(
#      load_github_packages(list('Ramd', 'robertzk')),
#      "client error")
#  })
#}
#

optional_tests <-
  c('lib/shared/default_adapter',
    'lib/shared/lexicals',
    'lib/stages/model'
  )

if (!nzchar(Sys.getenv('TRAVIS'))) ignored_tests <- 'models'

# Test setup hooks
setup <- define('check_readme', 'setup_import_data',
                function(check_readme, setup_import_data) {
  list("Setup import_data for models"       = setup_import_data(director, optional_tests),
       "Check existence of README.md files" = check_readme)
})

# Test teardown hooks
teardown <- list(
  "All tests ran" = function(env) {
    cat("All tests ran...\n")
  }
)

single_teardown <- list(
  "Hotfix option changes" = function(env) {
    # For some reason, the "mgcv.vc.logrange" global option is getting added on travis.
    options(mgcv.vc.logrange = NULL)
  }
)

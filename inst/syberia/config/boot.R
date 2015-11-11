if (interactive()) {

  if (resource_exists('config/global')) {
    # TODO: (RK) Create a separate namespace and inject into search path instead
    # of blatantly overwriting globalenv.

    globals <- resource('config/global')
    for (global in ls(globals))
      assign(global, globals[[global]], envir = globalenv()) # yuck
  }

}

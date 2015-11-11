Stages
===========

Central to the modeling process are several different key steps:

   * Import our data
   * Run data preparation
   * Run the actual model
   * Export the model
   * (Optional) Validate our results

To construct these individual components, we need to be able to translate parameters for our
modeling process (where the data is coming from, what data preparation we are running with
which parameters, what tuning parameters we use in our model, the location the model will be
exported to) into actual functions that perform the heavy lifting.

Anything in this directory is intended to take a list of options and translate them into
something suitable for inclusion in a stagerunner (a function, a list of functions, or a
sub-stagerunner).

Let's say we want to be able to write `import = "some_file"` in our [models](../controllers/models).
Then we can write the following `import.R` file in this directory:

```R
function(options) {
  # Options will just be the name of our file, like "some_file" -- it doesn't have to be a list!
  if (!is.character(options)) stop("Import stage options must be a filename.")

  function(modelenv) { # we are returning a function that will operate on our modeling environment
    modelenv$data <- read.csv(options)
  }
}
```

In this way, we can now simply write

```R
list(
  import = "some_file",
  ...
)
```

and put the important information in one place (the model file) while leaving the details to the
[`import` stage](import.R).

Import Stage
===========
   * TO-DO (Write)  

Data Stage
===========
   * TO-DO (Write)

Model Stage
===========
   * TO-DO (Write)

Evaluation Stage
===========
   * TO-DO (Write)

Export Stage
===========
   * TO-DO (Write)

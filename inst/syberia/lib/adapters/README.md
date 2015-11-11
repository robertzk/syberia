Adapters
======

Given the frequency with which we read data from R and write models out,
the ability to `$read` and `$write` deserves to be encapsulated with an object.

To construct your own custom adapter (for use in import stage), you could do something
like the following

**custom.R**
```R
read <- function(name) {
  cat("I am reading ", name, "...\n")
  NULL
}

write <- function(name) {
  cat("I am writing ", name, "...\n")
}
```

We can now use this in import or export stage.

```R
list(
  import = list(custom = 'some_data'),
  ...,
  export = list(custom = 'model_output')
)
```

and we will see the output `"I am reading some_data..."` and `"I am writing model_output..."`
(although otherwise being completely useless). (Typically, `$read` will return the value
of the data we are reading in, and `$write` will perform some operation that writes out
the object it is passed as its first argument.)

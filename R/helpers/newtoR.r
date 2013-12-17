newtoR <- function(x)
{ if (!require(x, character.only = TRUE)) {
  install.packages(x, dep = TRUE)
  if(!require(x, character.only = TRUE)) stop('Package not found') } }

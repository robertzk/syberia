# This should error, since utility1 is on the mounted engine,
# not on this (root) engine.
resource("hello", engine = "utility1")

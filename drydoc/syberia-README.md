Syberia is a collection of R packages that try to enforce [convention over configuration](http://en.wikipedia.org/wiki/Convention_over_configuration)
and [don't repeat yourself](http://en.wikipedia.org/wiki/Don't_repeat_yourself).

R codebases are typically loosely organized collections of scripts. By enforcing a structure that
encourages separating out components for re-use and enabling automated testing,
several long-term effects on the modeling process should emerge: research should be
reproducible, there should be no difference between experimenting with a new method
and developing something for production (i.e., development = production), and
complex interdependencies should be incapable of causing breakdowns as a result of
the inability of the developers to maintain such complexity.

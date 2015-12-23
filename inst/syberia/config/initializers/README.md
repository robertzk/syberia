Initializers
==========

As with [Rails initializers](http://guides.rubyonrails.org/configuring.html), any code
that is configuration and/or startup related for packages or plugins not related to the
core Syberia project should be placed in `config/initializers`. For example, if you
are using `knitr` or `Rmarkdown` with some custom settings that you would like to
not pollute the global options space with, you can place them in `config/initializers/knitr`
or `config/initializers/Rmarkdown` and access those resources with
`resource('config/initializers/knitr)` or `resource('config/initializers/Rmarkdown)` within
other Syberia resources (models or `lib` objects).

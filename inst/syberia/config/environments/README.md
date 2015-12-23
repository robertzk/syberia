Environments
--------

Files in this directory have the same purpose as the [`application.R`](../application.R) file in the
`config` directory upstairs, with
the exception that they are sourced only under certain circumstances.

For the moment being, this distinction is only made for the `test` environment.
When executing [`test_project`](https://github.com/robertzk/syberia/blob/master/R/tests.R),
it may appeal to `config/environments/test` for some of its configuration values.

The idea of including environment-specific configuration files was
[borrowed from Rails](http://guides.rubyonrails.org/configuring.html#creating-rails-environments).

*Note*: This directory has nothing to do with [R environments](http://adv-r.had.co.nz/Environments.html).


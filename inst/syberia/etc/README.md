Non-resources
===========

Almost every file in this Syberia project is a "resource", in the sense of being
maintained by a [director](https://github.com/robertzk/director) object, and
logically very tightly integrated with the rest of the project: mungebits and
classifiers are used in models, and stages are used to construct model stageRunners.

Borrowed from [the Linux operating system](http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/etc.html),
the `etc` directory is meant to contain scripts or configuration files
that are heavily used by auxiliary packages, but don't warrant inclusion
into `config` because they are not critical to configuring *the project*,
but only critical to *certain resources* in certain cases. (For example,
indicator variables are not relevant to the project as a whole, only the
import stage for some specific models).

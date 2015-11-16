Syberia [![Build Status](https://travis-ci.org/robertzk/syberia.svg?branch=master)](https://travis-ci.org/robertzk/syberia.svg?branch=master) [![Coverage Status](https://coveralls.io/repos/robertzk/syberia/badge.png)](https://coveralls.io/r/robertzk/syberia) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/syberia/) 
===========

Syberia is the development framework for R.

The original formulation, developed at [Avant](https://github.com/avantcredit), is
[the modeling grammar](http://github.com/syberia/modeling.sy)
that serves as a machine learning classifier development and deployment
framework for solving arbitrary academic, research and business problems
that require use of statistical methods.

The modeling engine provides an opinionated unified framework for
fast iteration on classifier development and deployment. It has 
modularity and testability built in as a design assumption, is
founded on convention-over-configuration, and aims to solve the
problems of classifier-specific data preparation and
classifier-specific modeling parameters.

The more general vision for Syberia is still in progress, but aims
to unify the currently disparate realms of R packages, script codebases,
Shiny dashboards, R web apps, and reproducible analysis. In the
viewpoint of [the author](https://github.com/robertzk),
R is syntactic sugar around LISP, which enables arbitrary computation;
Syberia is an attempt to support this conjecture by allowing the
construction of arbitrary software projects within the R programming language,
thereby finally outgrowing its long-overdue misconception as a statistical tool.

The [modeling engine](https://github.com/syberia/modeling.sy),
currently the main significant engine built in Syberia,
is a framework for building, debugging, testing, and deploying
classifiers developed in R. It includes support for importing data,
preparing data, arbitrary statistical modeling methodologies,
exporting trained classifiers, validating the results, and
deploying as a REST service. 

The timeline for future engines and information about how
to contribute is listed at the [Syberia roadmap](http://syberia.io/roadmap).

## Installation

To get started right away, try out the minimal
[example syberia project](https://github.com/syberia/example.sy):

```
# Run this from your command line terminal.
git clone git@github.com:syberia/example.sy.git && cd example.sy && R
```

This will open an R console (installing dependencies for the first time may take a while;
for troubleshooting see the [troubleshooting guide](http://syberia.io/trouble)).
You can then type:

```r
run("example1")
model$predict(iris[1:5, ]) # The first five scores from a trained classifier.
# [1] 5.005686 4.757667 4.773923 4.890092 5.055138
```

For more detailed instructions, see the [installation guide](http://syberia.io/install).

## Packages

Syberia relies on the following supplemental packages:

| Name | Status |
| ---- | -----  |
| [**Mungebits2**](https://github.com/robertzk/mungebits2) | [![Build Status](https://travis-ci.org/robertzk/mungebits2.svg?branch=master)](https://travis-ci.org/robertzk/mungebits2) [![Coverage Status](https://coveralls.io/repos/robertzk/mungebits2/badge.svg?branch=master&service=github)](https://coveralls.io/r/robertzk/mungebits2) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/mungebits2/) |
| [**Stagerunner**](https://github.com/robertzk/stagerunner) | [![Build Status](https://travis-ci.org/robertzk/stagerunner.svg?branch=master)](https://travis-ci.org/robertzk/stagerunner) [![Coverage Status](https://img.shields.io/coveralls/robertzk/stagerunner.svg)](https://coveralls.io/r/robertzk/stagerunner) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/stagerunner/) |
| [**Tundra**](https://github.com/robertzk/tundra) | [![Build Status](https://img.shields.io/travis/robertzk/tundra.svg)](https://travis-ci.org/robertzk/tundra.svg?branch=master) [![Coverage Status](https://img.shields.io/coveralls/robertzk/tundra.svg)](https://coveralls.io/r/robertzk/tundra) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/tundra/)  |
| [**Director**](https://github.com/robertzk/director) | [![Build Status](https://travis-ci.org/robertzk/director.svg?branch=master)](https://travis-ci.org/robertzk/director) [![Coverage Status](https://img.shields.io/coveralls/robertzk/director.svg)](https://coveralls.io/r/robertzk/director) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/director/) |

Additional packages used internally at [Avant](https://github.com/avantcredit)
in conjunction with Syberia modeling projects include
[batchman](https://github.com/peterhurford/batchman),
[bettertrace](https://github.com/robertzk/bettertrace),
[cachemeifyoucan](https://github.com/robertzk/cachemeifyoucan),
[dokk](https://github.com/kirillseva/dokk),
[lockbox](https://github.com/robertzk/lockbox),
[microserver](https://github.com/robertzk/microserver),
[objectdiff](https://github.com/robertzk/objectdiff),
[Ramd](https://github.com/robertzk/Ramd),
[rocco](https://github.com/robertzk/rocco),
[s3mpi](https://github.com/robertzk/s3mpi),
[testthatsomemore](https://github.com/robertzk/testthatsomemore), and
[treeskeleton](https://github.com/robertzk/treeskeleton).

### License

This project is licensed under the MIT License:

Copyright (c) 2014-2016 Robert Krzyzanowski

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Authors

Syberia was created at [Avant](https://github.com/avantcredit)
by Robert Krzyzanowski, rob@robertzk.com.


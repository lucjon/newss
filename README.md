# newss

[![Build Status](https://travis-ci.org/lucjon/newss.svg?branch=master)](https://travis-ci.org/lucjon/newss)
 [![codecov](https://codecov.io/gh/lucjon/newss/branch/master/graph/badge.svg)](https://codecov.io/gh/lucjon/newss)

This repository contains a new implementation of the Schreier-Sims algorithm
for computing stabilizer chains for permutation groups, as well as some related
algorithms, which does not rely on any of the GAP built-in stabilizer chain
(or orbit/stabilizer) functions.

To install the package, simply clone the repository to your GAP `pkg/`
directory. To compile the documentation, run `gap makedoc.g`, and to run the
test suite, run the `test/runtests` script.

Optionally, some parts of the library have been compiled to C for performance
reasons. If you would like to compile the kernel module, simply run

    make

from the package directory. Note that if you are not compiling in the directory
`GAP/pkg/newss`, then you must set the `GAPROOT` variable, e.g.

    make GAPROOT=~/gap

for the compilation to succeed.

# newss

[![Build Status](https://travis-ci.org/lucjon/newss.svg?branch=master)](https://travis-ci.org/lucjon/newss)
 [![codecov](https://codecov.io/gh/lucjon/newss/branch/master/graph/badge.svg)](https://codecov.io/gh/lucjon/newss)

This repository contains a new, pure-GAP implementation of the Schreier-Sims
algorithm for computing stabilizer chains for permutation groups, as well as
some related algorithms, which does not rely on any of the GAP built-in
stabilizer chain (or orbit/stabilizer) functions.

To install the package, simply clone the repository to your GAP `pkg/`
directory. To compile the documentation, run `gap makedoc.g`, and to run the
test suite, run the `test/runtests` script.

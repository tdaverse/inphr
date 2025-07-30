# inphr 0.0.1

This is a new submission to CRAN.

## Goals

The [{inphr}](https://tdaverse.github.io/inphr/) package is intended to be a
package for making inference on samples of persistence homology data. It is 
part of the *TDAverse* suite of packages, which are designed to provide a
collection of packages for enabling machine learning and data science tasks
using persistent homology.

## Current features

The package currently exposes two main functions which test if two samples of PH data have been generated from the same distribution:

- [`two_sample_diagram_test()`](https://tdaverse.github.io/inphr/reference/two_sample_diagram_test.html) works in the space of diagrams, using test statistics based on inter-point distances only.
- [`two_sample_functional_test()`](https://tdaverse.github.io/inphr/reference/two_sample_functional_test.html) works in a functional space (one of Betti, Euler characteristic, normalized life, silhouette or entropy) and uses interval-wise testing (providing strong control of familywise error rate) to output on which portions of the scale sequence does the difference occur.

## Dependencies

Messages, warnings and errors are relayed to the user using the
[{rlang}](https://rlang.r-lib.org) package and the
[{cli}](https://cli.r-lib.org) package which are both licensed under the MIT
license and with no dependency trail.

Inference in the space of diagrams is performed thanks to the combination of
[{phutil}](https://cran.r-project.org/package=phutil) which computes distances between diagrams in an efficient manner and
[{flipr}](https://cran.r-project.org/package=flipr) which powers the permutation schemes and test statistics based on
inter-point distances.

Inference in functional spaces is performed thanks to the combination of
[{TDAvec}](https://cran.r-project.org/package=TDAvec) which provides the suitable PH vectorization and [{fdatest}](https://cran.r-project.org/package=fdatest) which
powers the interval-wise testing procedure for functional data.

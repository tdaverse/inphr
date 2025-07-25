
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inphr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tdaverse/inphr/graph/badge.svg)](https://app.codecov.io/gh/tdaverse/inphr)
[![R-CMD-check](https://github.com/tdaverse/inphr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tdaverse/inphr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of [{inphr}](https://tdaverse.github.io/inphr/) is to provide a
set of functions for performing null hypothesis testing on samples of
persistence diagrams using the theory of permutations. Currently, only
two-sample testing is implemented. Inputs can be either samples of
persistence diagrams themselves or vectorizations. In the former case,
they are embedded in a metric space using either the Bottleneck or
Wasserstein distance. In the former case, persistence data becomes
functional data and inference is performed using tools available in the
[{fdatest}](https://permaverse.github.io/fdatest/) package.

## Installation

You can install the development version of inphr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tdaverse/inphr")
```

## Example

Let us start by loading the package:

``` r
library(inphr)
```

The package contains three sets of persistence diagrams, which can be
used for testing. They are available in the package as `trefoils1`,
`trefoils2`, and `archspirals`. The first two sets contain persistence
diagrams computed from noisy samples of trefoil knots, while the third
set contains persistence diagrams computed from noisy samples of 2-armed
Archimedean spirals. Each set contains 24 persistence diagrams, each
computed from a sample of 120 points sampled from the respective shape,
with Gaussian noise added (standard deviation = 0.05). The persistence
diagrams were computed using the
[`TDA::ripsDiag()`](https://www.rdocumentation.org/packages/TDA/versions/1.9.1/topics/ripsDiag)
function with a maximum scale of 6 and up to dimension 2.

You can use the
[`two_sample_test()`](https://tdaverse.github.io/inphr/reference/two_sample_test.html)
function to perform a two-sample test on these persistence diagrams. For
example, to test whether the first 5 persistence diagrams from the first
set are significantly different from the first 5 persistence diagrams
from the second set, you can run:

``` r
two_sample_test(trefoils1[1:5], trefoils2[1:5], B = 100L)
#> [1] 0.9782132
```

To test whether the first 5 persistence diagrams from the first set are
significantly different from the first 5 persistence diagrams from the
third set, you can run:

``` r
two_sample_test(trefoils1[1:5], archspirals[1:5], B = 100L)
#> [1] 0.008047755
```

## Contributions

### Code of Conduct

Contributions are welcome! Please feel free to open an issue or a pull
request if you have any suggestions or improvements. The package is
still in its early stages, so any feedback is appreciated.

Please note that the {inphr} project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

### Acknowledgements

This project was funded by [an ISC grant from the R
Consortium](https://r-consortium.org/all-projects/2024-group-1.html#modular-interoperable-and-extensible-topological-data-analysis-in-r)
and done in coordination with Jason Cory Brunson and with guidance from
Bertrand Michel and Paul Rosen. It builds upon conversations with
Mathieu Carrière and Vincent Rouvreau who are among the authors of the
[GUDHI](https://gudhi.inria.fr) library. Package development also
benefited from the support of colleagues at the [Department of
Mathematics Jean Leray](https://www.math.sciences.univ-nantes.fr) and
the use of equipment at [Nantes
University](https://english.univ-nantes.fr).

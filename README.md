<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/semlrtp.svg)](https://github.com/sfcheung/semlrtp)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/semlrtp.svg)](https://github.com/sfcheung/semlrtp/commits/main)
[![R-CMD-check](https://github.com/sfcheung/semlrtp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/semlrtp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.0, updated on 2024-06-18, [release history](https://sfcheung.github.io/semlrtp/news/index.html))

# semlrtp: Likelihood Ratio Test p-Values for Structural Equation Models <img src="man/figures/logo.png" align="right" height="150" />

This package is used for computing *LRT* *p*-*values*
for free parameters
in structural equation model using likelihood ratio test (LRT).
As demonstrated in the [Get-Started article](https://sfcheung.github.io/semlrtp/articles/semlrtp.html),
the LRT *p*-value has some advantages over the usual *p*-values
reported in SEM program.

However, it is not easy to compute
them because each *p*-value requires fitting one additional
model. We developed this package to make this process simple
and automatic such that it is practical to use LRT *p*-values
in most commonly interpreted parameters.

See [the Get-Started article](https://sfcheung.github.io/semlrtp/articles/semlrtp.html)
for a quick demonstration on how to use this package.

For more information on this package, please visit its GitHub page:

https://sfcheung.github.io/semlrtp/

# Installation

The latest developmental version of this package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/semlrtp")
```

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue [here](https://github.com/sfcheung/semlrtp/issues).
Thanks.
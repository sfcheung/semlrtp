# semlrtp: Likelihood Ratio Test p-Values for Structural Equation Models

(Version 0.1.2, updated on 2026-07-19, [release
history](https://sfcheung.github.io/semlrtp/news/index.html))

This package is used for computing *LRT* *p*-*values* for free
parameters in structural equation model using likelihood ratio test
(LRT). As demonstrated in the [Get-Started
article](https://sfcheung.github.io/semlrtp/articles/semlrtp.html), the
LRT *p*-value has some advantages over the usual *p*-values reported in
SEM program.

However, it is not easy to compute them because each *p*-value requires
fitting one additional model. We developed this package to make this
process simple and automatic such that it is practical to use LRT
*p*-values in most commonly interpreted parameters.

See [the Get-Started
article](https://sfcheung.github.io/semlrtp/articles/semlrtp.html) for a
quick demonstration on how to use this package.

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/semlrtp/>

# Installation

The stable version at CRAN can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r

install.packages("semlrtp")
```

The latest developmental version of this package can be installed by
`remotes::install_github`:

``` r

remotes::install_github("sfcheung/semlrtp")
```

# Issues

If you have any suggestions and found any bugs, please feel feel to open
a GitHub issue [here](https://github.com/sfcheung/semlrtp/issues).
Thanks.

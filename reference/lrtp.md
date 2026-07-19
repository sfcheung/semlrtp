# Likelihood Ratio Test P-Values

Compute the likelihood ratio test (LRT) *p*-values for free parameters
in a `lavaan` output.

## Usage

``` r
lrtp(
  fit,
  op = c("~", "~~"),
  no_variances = TRUE,
  no_error_variances = TRUE,
  no_error_covariances = TRUE,
  se_keep_bootstrap = FALSE,
  LRT_method = "default",
  scaled.shifted = TRUE,
  fallback_method = "satorra.2000",
  progress = TRUE,
  parallel = FALSE,
  ncores = parallel::detectCores(logical = FALSE) - 1,
  load_balancing = TRUE,
  ...
)
```

## Arguments

- fit:

  A `lavaan`-class object.

- op:

  A character vector of `lavaan` model syntax operators. Free parameters
  of these operators will be included, unless excluded by other
  arguments. Default is `c("~", "~~")`.

- no_variances:

  Logical. If `TRUE`, the default, then all free variances are excluded.
  (Error variances are handled by `no_error_variances`.)

- no_error_variances:

  Logical, If `TRUE`, the default, then all free error variances are
  excluded.

- no_error_covariances:

  Logical. If `TRUE`, the default, then all free error covariances are
  excluded.

- se_keep_bootstrap:

  Logical. If `TRUE` and `fit` used bootstrapping standard error (with
  `se = "bootstrap"`), bootstrapping will also be use in fitting the
  restricted model. If `FALSE`, the default, then `se` will be set to
  `"standard"` if it is `"bootstrap"` in `fit`, to speed up the
  computation.

- LRT_method:

  String. Passed to the `method` argument of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Default is `"default"`, and let
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  decide the method based on `fit`.

- scaled.shifted:

  Logical. Used when the method used in
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  is `"satorra.2000"`. Default is `TRUE` and a scaled and shifted test
  statistic is used, the same default of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

- fallback_method:

  The default method of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html),
  `"satorra.bentler.2001"`, may sometimes fail. If failed, this function
  will call
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  again using `fallback_method`. which is `"satorra.2000"` by default.

- progress:

  Logical. If `TRUE`, the default, a progress bar will be displayed to
  show the progress (using the `pbapply` package).

- parallel:

  Logical. If `TRUE`, parallel processing will be used to compute the
  LRT *p*-values for selected parameters. Default is `FALSE`. Set it to
  `TRUE` if the number of selected parameters is large.

- ncores:

  Integer. The number of CPU cores to use if `parallel` is `TRUE`.
  Default is the number of physical cores (determined by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html))
  minus 1.

- load_balancing:

  Logical. If `TRUE`, the default, and `parallel` is `TRUE`, then load
  balancing will be used. May shorten the total processing time if the
  time to compute LRT *p*-values vary a lot across parameters.

- ...:

  Optional arguments to be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).

## Value

An `lrt`-class object, which is a data-frame-like object similar to the
output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
with a column `LRTp` for the LRT *p*-values, as well as other columns
such as the chi-square difference in the test. it has a print method,
[`print.lrtp()`](https://sfcheung.github.io/semlrtp/reference/print.lrtp.md).

## Details

It finds free parameters in a `lavaan`-class object, computes the
likelihood ratio test (LRT) *p*-value for each of them when fixed to
zero, and returns a parameter estimates table with the LRT *p*-values
included.

By default, it only computes LRT *p*-values for regression paths and
covariances, except for error covariances. This default can be
overridden using arguments such as `op`, `no_variances`,
`no_error_variances`, and `no_error_covariances`.

### Technical Details

It first identify the parameters to be processed, and then call
[`lrt()`](https://sfcheung.github.io/semlrtp/reference/lrt.md) on each
of them. Please refer to
<https://sfcheung.github.io/semlrtp/articles/internal_workflow.html> for
the internal workflow.

## See also

[`print.lrtp()`](https://sfcheung.github.io/semlrtp/reference/print.lrtp.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

library(lavaan)
data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f4 =~ x10 + x11 + x12
f2 ~~ f1
f3 ~ f1 + f2
f4 ~ f3
"
fit <- sem(mod, data_sem16)
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: f2 (x4 -> 
#>    x6)
lrtp(fit)
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f3 ~                                                
#>     f1                0.248    0.097   10.343    0.001
#>     f2                0.090    0.056    3.778    0.052
#>   f4 ~                                                
#>     f3                2.118    0.684   86.205    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 ~~                                               
#>     f2                0.127    0.046    9.717    0.002
#> 
lrtp(fit, op = "~")
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f3 ~                                                
#>     f1                0.248    0.097   10.343    0.001
#>     f2                0.090    0.056    3.778    0.052
#>   f4 ~                                                
#>     f3                2.118    0.684   86.205    0.000
#> 
```

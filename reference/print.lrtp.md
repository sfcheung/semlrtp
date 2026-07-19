# Print an 'lrtp'- Class Object

Print the content of an `lrtp`-class object.

## Usage

``` r
# S3 method for class 'lrtp'
print(
  x,
  digits = 3,
  lrtp_only = TRUE,
  wald_stats = FALSE,
  output = c("text", "data.frame", "table"),
  ...
)
```

## Arguments

- x:

  An `lrtp`-class object.

- digits:

  Integer. The number of decimal places to print. Default is 3.

- lrtp_only:

  Logical. If `TRUE`, the default, only parameters with LRT *p*-values
  will be printed.

- wald_stats:

  Logical. If `TRUE`, the usual Wald statistics (e.g., z statistics,
  *p*-values, CIs) are printed. `FALSE` by default, assuming that users
  prefer using LRT statistics when using
  [`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md).

- output:

  The format of the printout. If `"text"`, then the style in the
  [`summary()`](https://rdrr.io/r/base/summary.html) of the
  `lavaan`-class object is used. If `"data.frame"` or `"table"`, then
  the data frame format of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  is used.

- ...:

  Optional arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The print method for the output of
[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md).

Additional diagnostic information will be printed if one or more
likelihood tests encounter some errors or warnings.

## See also

[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md)

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
"
fit <- sem(mod, data_sem16)
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: f2 (x4 -> 
#>    x6)

out <- lrtp(fit)
out
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Covariances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 ~~                                               
#>     f2                0.134    0.049   10.646    0.001
#> 
print(out, lrtp_only = FALSE)
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 =~                                               
#>     x1                1.000                NA       NA
#>     x2                0.963    0.252       NA       NA
#>     x3                1.054    0.284       NA       NA
#>   f2 =~                                               
#>     x4                0.422    0.189       NA       NA
#>     x5                0.748    0.292       NA       NA
#>     x6                1.000                NA       NA
#> 
#> Covariances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 ~~                                               
#>     f2                0.134    0.049   10.646    0.001
#> 
#> Variances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>    .x1                1.020    0.111       NA       NA
#>    .x2                1.048    0.109       NA       NA
#>    .x3                0.709    0.100       NA       NA
#>    .x4                1.154    0.096       NA       NA
#>    .x5                0.972    0.114       NA       NA
#>    .x6                0.826    0.162       NA       NA
#>     f1                0.276    0.100       NA       NA
#>     f2                0.375    0.163       NA       NA
#> 
```

# Fix a Free Parameter To Zero And Fit a Model Again

It fixes a designated free parameter in a lavaan object to zero and
refit the model.

## Usage

``` r
fix_to_zero(fit, par_id, store_fit = FALSE, se_keep_bootstrap = FALSE)
```

## Arguments

- fit:

  A `lavaan`-class object.

- par_id:

  An integer. The row number of the free parameter in the parameter
  table of `fit` to be fixed.

- store_fit:

  Logical. If `TRUE`, `fit` will be stored in the output. Default is
  `FALSE`.

- se_keep_bootstrap:

  Logical. If `TRUE` and `fit` used bootstrapping standard error (with
  `se = "bootstrap"`), bootstrapping will also be use in fitting the
  restricted model. If `FALSE`, the default, then `se` will be set to
  `"standard"` if it is `"bootstrap"` in `fit`, to speed up the
  computation.

## Value

A `fix_to_zero`-class object, which is a list with these elements:

- `fit0` is the `lavaan` output of the refitted object. `NA` if the fit
  failed for some reasons. To be considered an acceptable solution, the
  optimization must converge, the solution passes `lavaan`'s post check,
  the variance-covariance matrix of estimates successfully computed, and
  the increase in the model degree of freedom equal to the expected
  change.

- `fit1` is the original `lavaan` output if `store_fit` is `TRUE`. It is
  `NULL` if `store_fit` is `FALSE`, the default.

- `par_id` is the row number of the designated free parameter in the
  parameter table.

- `call` is the original call to this function.

- `ptable0` is the parameter table with the designated parameter fixed
  to zero. It can be used for diagnostic purpose if the fit failed.

- `fit0_error` is the error message in refitting the model (`ptable0`),
  if any. If no error, it is `NA`.

- `vcov_ok` is `TRUE` if the variance-covariance matrix of the estimates
  can be computed without error nor warning. `FALSE` otherwise.

- `vcov_msg` is the message generated when using
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  to get the variance-covariance matrix of the parameter estimates of
  the refitted model. If `TRUE`, then no error nor warning. Can be used
  for diagnostic purposes.

- `converged`: Whether refitting the modified model (`ptable0`)
  converged.

- `post_check_passed`: Whether the solution of the modified model
  (`ptable0`) passed `lavaan`'s post check.

- `post_check_msg`: If the solution failed `lavaan`'s post check, it
  stores the warning message. If the solution passes the check, it is
  `NA`.

- `fit_not_ok`: If the fit failed for some reasons, the fit object, if
  available, is stored in this element rather than in `fit0`. such that
  the fit object can be retrieved for diagnostic purposed if necessary.

- `df_diff_one`: Whether the difference in model degrees of freedom
  between the modified model and the original model is one. If a
  variance is fitted to zero, related covariance(s) is/are also fitted
  to zero and so the difference in model degrees of freedom can be
  legitimately greater than one.

- `se_force_standard`: Whether `se` was forced to be `"standard"` even
  if it is `"bootstrap"` in `fit`. If `FALSE`, then either `se` is not
  `"bootstrap"` in `fit` or it was not changed in fitting the restricted
  model.

## Details

It modifies the parameter table of a `lavaan`-class object and then fits
the model again.

Users should usually call
[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md)
directly instead of calling this function. It is exported for
developers.

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
# Fix the factor covariance to zero
out <- fix_to_zero(fit, par_id = 15)
summary(out$fit0)
#> lavaan 0.7-2 ended normally after 36 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        12
#> 
#>   Number of observations                           336
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                27.655
#>   Degrees of freedom                                 9
#>   P-value (Chi-square)                           0.001
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 =~                                               
#>     x1                1.000                           
#>     x2                0.911    0.253    3.604    0.000
#>     x3                1.051    0.323    3.257    0.001
#>   f2 =~                                               
#>     x4                0.181    0.207    0.873    0.383
#>     x5                0.459    0.519    0.885    0.376
#>     x6                1.000                           
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 ~~                                               
#>     f2                0.000                           
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                1.010    0.119    8.457    0.000
#>    .x2                1.067    0.111    9.573    0.000
#>    .x3                0.700    0.114    6.159    0.000
#>    .x4                1.198    0.096   12.500    0.000
#>    .x5                1.037    0.182    5.705    0.000
#>    .x6                0.515    0.775    0.664    0.507
#>     f1                0.286    0.110    2.595    0.009
#>     f2                0.687    0.779    0.882    0.378
#> 
```

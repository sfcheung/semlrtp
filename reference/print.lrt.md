# Print an 'lrt'- Class Object

Print the content of an `lrt`-class object.

## Usage

``` r
# S3 method for class 'lrt'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An `lrt`-class object.

- digits:

  Integer. The number of decimal places to print. Default is 3.

- ...:

  Optional arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

It is the print method for the output of
[`lrt()`](https://sfcheung.github.io/semlrtp/reference/lrt.md).

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

out <- lrt(fit, par_id = "f1 ~~ f2")
out
#> 
#> ==== LRT p-value ====
#> 
#> Parameter: f1~~f2 
#> 
#> LRT test with the selected parameter fixed to zero:
#> 
#> Chi-Squared Difference Test
#> 
#>      Df  AIC  BIC Chisq Chisq diff RMSEA Df diff Pr(>Chisq)   
#> fit   8 6024 6074  17.0                                       
#> fit0  9 6033 6079  27.6       10.7 0.169       1     0.0011 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```

# Get Started

## Introduction

This article illustrates how to use
[`semlrtp`](https://sfcheung.github.io/semlrtp/) to compute LRT
*p*-values for selected free parameters in a model fitted by `lavaan`.

These are the packages needed for this illustration:

``` r

library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
library(semlrtp)
```

## LRT *p*-Values

What we called the LRT *p*-value of a parameter is simply the *p*-value
of the likelihood ratio test (LRT, a.k.a. $`\chi^2`$ difference test)
comparing the fitted model with the model with this parameter fixed to
zero. It is not new but we are not aware of packages using it as the
default *p*-value in testing model parameters. The package
[`semlrtp`](https://sfcheung.github.io/semlrtp/) is developed to
facilitate the use of this *p*-value.

## Basic Workflow

### Data

This is the sample dataset from the package, with 16 variables and a
group variable:

``` r

data(data_sem16)
print(head(data_sem16), digits = 2)
#>      x1    x2    x3    x4     x5     x6    x7    x8    x9   x10   x11    x12
#> 1  1.00 -1.43  0.45 -0.67 -0.044 -1.316 -0.52 -0.35 -1.01  1.40 -0.22 -1.879
#> 2  0.07 -0.45  1.85 -0.68  0.524 -0.172  0.52 -2.46 -0.34 -0.20 -0.55 -0.948
#> 3  0.95  0.24  0.07  0.69 -0.726  0.164 -0.55 -0.69 -1.36  0.36  0.16  0.758
#> 4  0.28  0.92  0.29 -0.93 -1.516 -0.047 -0.37  0.69  0.38  0.19  0.21  1.089
#> 5 -2.10 -0.55 -1.22 -1.57 -0.886  1.405  1.28  1.00 -1.74 -0.96 -1.51 -0.062
#> 6  1.01  1.05  0.93  0.70 -0.380  1.513 -1.97  1.44  2.42  1.45  0.60  0.521
#>      x13   x14    x15    x16 group
#> 1 -0.110 -0.89  0.572  0.093 gamma
#> 2 -0.036 -1.65  0.142 -0.506 gamma
#> 3 -0.562 -0.53  0.508  1.549 gamma
#> 4 -0.885  0.93 -0.824  2.374 gamma
#> 5  0.076  1.16 -0.011  0.491 gamma
#> 6  0.830  0.45  1.886  1.489 gamma
```

### Model

This is a model to be fitted, with four latent factors, and a structural
model for the factors:

``` r

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x5 + x6 + x7
f3 =~ x9 + x10 + x11
f4 =~ x13 + x14 + x15
f3 ~ f1 + f2
f4 ~ f3
"
```

We first fit a model to the whole sample:

``` r

fit <- sem(mod,
           data_sem16)
```

### LRT *p*-Values For Selected Parameters

If we use the default settings, we can compute the LRT *p*-values just
by calling
[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md) on the
`lavann` output.

By default, LRT *p*-values will be computed only for regression paths
(`"~"`) and covariances (`"~~"`), excluding variances and error
covariances.

``` r

fit_lrtp <- lrtp(fit)
```

By default, the output will be printed in `lavaan` style, and only
parameters with LRT *p*-values are printed:

``` r

fit_lrtp
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
#>     f1                0.656    0.191   13.381    0.000
#>     f2                0.347    0.179    4.016    0.045
#>   f4 ~                                                
#>     f3                0.602    0.131   44.155    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 ~~                                               
#>     f2                0.099    0.039    9.729    0.002
```

The column `Chisq` shows the $`\chi^2`$ difference of the likelihood
ratio test when a parameter is fixed to zero.

The column `LRTp` shows the LRT *p*-value.

### How LRT *p*-Values Are Computed

It can be verified that the LRT *p*-value of a parameter is the
likelihood ratio test (LRT) *p*-value (a.k.a. the $`\chi^2`$ difference
test) when this parameter is fitted to zero.

For example, we fix the path from `f2` to `f3` to zero and then do an LR
test:

``` r

mod_f3_f2 <-
"
f1 =~ x1 + x2 + x3
f2 =~ x5 + x6 + x7
f3 =~ x9 + x10 + x11
f4 =~ x13 + x14 + x15
f3 ~ f1 + 0*f2
f4 ~ f3
"
fit_f3_f2 <- sem(mod_f3_f2,
                 data_sem16)
lavTestLRT(fit_f3_f2,
           fit)
#> 
#> Chi-Squared Difference Test
#> 
#>           Df   AIC   BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)  
#> fit       50 12206 12313 41.183                                         
#> fit_f3_f2 51 12208 12311 45.199      4.016 0.094743       1    0.04507 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Unlike the original *p*-value of this path, 0.052, the LRT *p*-value is
0.045, suggesting that the path from `f2` to `f2` is significant.

## Why LRT *p*-Value

The example above illustrates the importance of the LRT *p*-value. The
usual *p*-values in `lavaan` (and many other SEM programs) are
Wald-based *p*-values. The Wald-based p-value is an approximation of the
LRT *p*-value when a parameter is fixed to zero. It is an approximation
and so can be different from the LRT *p*-value. Moreover, they may also
depend on the parameterization (Gonzalez & Griffin, 2001).

For example, we can fit the same model by changing the indicators being
fixed to 1.

``` r

mod2 <-
"
f1 =~ x2 + x1 + x3
f2 =~ x7 + x5 + x6
f3 =~ x11 + x9 + x10
f4 =~ x14 + x13 + x15
f3 ~ f1 + f2
f4 ~ f3
"
fit2 <- sem(mod2,
            data_sem16)
```

This model and the previous one have exactly identical model fit, as
expected:

``` r

fitMeasures(fit, c("chisq", "df"))
#>  chisq     df 
#> 41.183 50.000
fitMeasures(fit2, c("chisq", "df"))
#>  chisq     df 
#> 41.183 50.000
```

This is the parameter estimates with Wald *p*-values (only those for the
regression paths are displayed)

``` r

parameterEstimates(fit2,
                   output = "text")
```

    #> Regressions:
    #>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
    #>   f3 ~                                                                  
    #>     f1                0.714    0.220    3.239    0.001    0.282    1.146
    #>     f2                0.505    0.276    1.828    0.067   -0.036    1.047
    #>   f4 ~                                                                  
    #>     f3                0.683    0.148    4.599    0.000    0.392    0.974

The Wald *p*-value is 0.067, even larger than 0.052 in the original
model.

However, the LRT *p*-values are the same:

``` r

fit2_lrtp <- lrtp(fit2)
fit2_lrtp
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
#>     f1                0.714    0.220   13.381    0.000
#>     f2                0.505    0.276    4.016    0.045
#>   f4 ~                                                
#>     f3                0.683    0.148   44.155    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err    Chisq     LRTp
#>   f1 ~~                                               
#>     f2                0.053    0.024    9.729    0.002
```

It is because LRT *p*-value is invariant to parameterization.

## Limitations

Because LRT *p*-value are computed by fixing a parameter to zero, there
are parameter for which the LRT *p*-value cannot be computed. For
example, suppose we request LRT *p*-values for factor loadings using the
argument `op`:

``` r

fit_lrtp_loadings <- lrtp(fit,
                          op = "=~")
fit_lrtp_loadings
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
#>     x2                0.844    0.206   27.682    0.000
#>     x3                1.145    0.255   54.842    0.000
#>   f2 =~                                               
#>     x6                1.127    0.331   33.687    0.000
#>     x7                0.630    0.207   13.364    0.000
#>   f3 =~                                               
#>     x10               0.823    0.143   50.232    0.000
#>     x11               0.918    0.151   60.791    0.000
#>   f4 =~                                               
#>     x14               1.043    0.240   40.668    0.000
#>     x15               0.475    0.152   12.393    0.000
```

As shown above, LRT *p*-values are not computed for indicators with
loadings fixed to zero.

## Further Information

Please refer to the help page of
[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md) for
other arguments, and the print method of
[`lrtp()`](https://sfcheung.github.io/semlrtp/reference/lrtp.md) output
([`print.lrtp()`](https://sfcheung.github.io/semlrtp/reference/print.lrtp.md))
for options in printing.

## References

Gonzalez, R., & Griffin, D. (2001). Testing parameters in structural
equation modeling: Every "one" matters. *Psychological Methods*, *6*(3),
258–269. <https://doi.org/10.1037//1082-989X.6.3.258>

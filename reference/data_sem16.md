# Sample Dataset For Test (16 Items and 2 Groups)

A 16-variable dataset with 336 cases.

## Usage

``` r
data_sem16
```

## Format

A data frame with 336 rows and 16 variables:

- x1:

  Indicator. Numeric.

- x2:

  Indicator. Numeric.

- x3:

  Indicator. Numeric.

- x4:

  Indicator. Numeric.

- x5:

  Indicator. Numeric.

- x6:

  Indicator. Numeric.

- x7:

  Indicator. Numeric.

- x8:

  Indicator. Numeric.

- x9:

  Indicator. Numeric.

- x10:

  Indicator. Numeric.

- x11:

  Indicator. Numeric.

- x12:

  Indicator. Numeric.

- x13:

  Indicator. Numeric.

- x14:

  Indicator. Numeric.

- x15:

  Indicator. Numeric.

- x16:

  Indicator. Numeric.

- group:

  Group with two values, "alpha" and "gamma". Character.

## Examples

``` r
library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
f3 ~ f2 + f1
f4 ~ f3
"
fit <- sem(mod, data_sem16)
summary(fit)
#> lavaan 0.7-2 ended normally after 53 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        36
#> 
#>   Number of observations                           336
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                86.336
#>   Degrees of freedom                               100
#>   P-value (Chi-square)                           0.833
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
#>     x2                0.888    0.200    4.446    0.000
#>     x3                1.049    0.216    4.863    0.000
#>     x4                0.582    0.168    3.470    0.001
#>   f2 =~                                               
#>     x5                1.000                           
#>     x6                1.202    0.328    3.667    0.000
#>     x7                0.792    0.240    3.294    0.001
#>     x8                0.407    0.188    2.168    0.030
#>   f3 =~                                               
#>     x9                1.000                           
#>     x10               0.854    0.135    6.343    0.000
#>     x11               0.920    0.139    6.634    0.000
#>     x12               0.814    0.133    6.114    0.000
#>   f4 =~                                               
#>     x13               1.000                           
#>     x14               1.077    0.215    5.008    0.000
#>     x15               0.475    0.143    3.319    0.001
#>     x16               0.939    0.193    4.857    0.000
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f3 ~                                                
#>     f2                0.455    0.208    2.185    0.029
#>     f1                0.555    0.174    3.195    0.001
#>   f4 ~                                                
#>     f3                0.561    0.115    4.887    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 ~~                                               
#>     f2                0.109    0.038    2.864    0.004
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                0.999    0.099   10.059    0.000
#>    .x2                1.070    0.099   10.849    0.000
#>    .x3                0.689    0.083    8.334    0.000
#>    .x4                1.120    0.092   12.110    0.000
#>    .x5                0.965    0.098    9.832    0.000
#>    .x6                0.888    0.110    8.055    0.000
#>    .x7                1.037    0.093   11.178    0.000
#>    .x8                1.006    0.081   12.487    0.000
#>    .x9                0.961    0.097    9.862    0.000
#>    .x10               1.010    0.093   10.817    0.000
#>    .x11               0.941    0.091   10.283    0.000
#>    .x12               1.069    0.096   11.128    0.000
#>    .x13               1.107    0.111   10.006    0.000
#>    .x14               1.113    0.117    9.541    0.000
#>    .x15               1.073    0.087   12.292    0.000
#>    .x16               1.064    0.104   10.255    0.000
#>     f1                0.297    0.089    3.322    0.001
#>     f2                0.217    0.083    2.617    0.009
#>    .f3                0.300    0.077    3.881    0.000
#>    .f4                0.200    0.070    2.866    0.004
#> 
```

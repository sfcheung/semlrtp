# semlrtp 0.0.0.9019

- Initialized the package. (0.0.0.9000)
- Setup `pkgdown` site. (0.0.0.9001)
- Added a dataset for testing.
- Added `free_pars()` (an internal function),
  with tests. (0.0.0.9002)
- Added `fix_to_zero()`, with tests.
  (0.0.0.9002)
- Added `lrt()`, with tests. (0.0.0.9003)
- Added `lrtp()` and its print method,
  with tests. (0.0.0.9004)
- Added a vignette, `Get Started`.
  (0.0.0.9005)
- Fixed issues with vignettes.
  (0.0.0.9006)
- Moved "TODOs" to issues. (0.0.0.9007)
- By default, the method `print.lrtp()`
  will not print Wald statistics
  (z statistics, Wald p-values,
  Wald confidence intervals).
  (0.0.0.9008)
- The chi-squared difference for each
  LRT *p*-value will also be printed.
  (0.0.0.9009)
- Fixed a bug with inserting columns
  in `lrtp()`. (0.0.0.9010)
- `free_pars()` can identify apparently
  free parameters which are actually
  fixed due to an equality constraint
  on a constant. (0.0.0.9011)
- Updated `fix_to_zero()` to check
  whether the requested parameter is
  successfully fixed to zero.
  (0.0.0.9012)
- Updated `fix_to_zero()` to handle
  parameters involved in internal a
  equality constraint (e.g, constrained
  to be equal between groups by
  the `group.equal` argument of
  `lavaan::lavaan()`). (0.0.0.9013)
- Updated `fix_to_zero()` to handle
  labelled parameters. (0.0.0.9014)
- Updated `fix_to_zero()` to check the
  VCOV of the modified model. If failed
  to get VCOV, the modified model may
  not be identified. (0.0.0.9015)
- Updated `lrt()`, `lrtp()`, and
  `print.lrtp()` to handle error in
  doing the LR test. (0.0.0.9016)
- Updated the theme of the `pkgdown`
  site. (0.0.0.9017)
- Revised several functions such that
  problems in fitting the modified model
  are recorded, instead of raising an
  error. The structure of the output
  of `lrt()` and `fix_to_zero()` are also
  modified to include indicators of
  error. To avoid unnecessarily storing
  many copies of the original fit
  object, a change is made in the output
  of `lrt()` which will some times break
  code for previous versions of
  `semlrtp`. (0.0.0.9018)
- `no_variances` in `lrtp()` revised
  to handle only variances, not
  error variances. (0.0.0.9019)

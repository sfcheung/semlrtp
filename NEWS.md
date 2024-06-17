# semlrtp 0.0.1.8

- First stable release. (0.0.1.0)
- Revised `fix_to_zero()`, `lrt()`, and
  `lrtp()` to accept a solution that
  failed the post.check test of
  `lavaan`. (0.0.1.1)
- Detailed diagnostic information and
  instruction will be provided if any
  of the likelihood ratio tests
  encountered any error or warning.
  (0.0.1.1)
- By default, `se = "bootstrap"` is used
  in the original solution, it will
  be forced to `se = "standard"` to
  save processing time. This can be
  turned off by the argument
  `se_keep_bootstrap`. (0.0.1.2)
- Updated `lrt()` and `lrtp()` to work
  with models fitted by methods like
  `MLR` and `MLM`. (0.0.1.3)
- Updated the `pkgdown` site. (0.0.1.4)
- Minor change in README.md. (0.0.1.5)
- Added a logo. (0.0.1.6)
- Added support for user-defined
  parameters. (0.0.1.7)
- Added support for parallel processing
  in computing LRT *p*-values.
  (0.0.1.8)
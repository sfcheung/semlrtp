library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f3 ~ f2 + f1
"
fit <- sem(mod, data_sem16, group = "group",
           group.equal = "regressions")


test_that("Factor covariances", {
  expect_no_error(fix_to_zero(fit, par_id = 59))
  expect_no_error(fix_to_zero(fit, par_id = 13))
})

# Need to add tests for
# Free parameters involved in any constraints
# Free parameters already labelled
# Free parameters used in user-defined variable(s)

library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

mod <-
"
f1 =~ x1 + c(a, a)*x2 + x3 + c(b1, b2)*x4
f2 =~ x5 + x6 + x7 + c(b1, d2)*x8
f3 =~ x9 + x10 + x11 + x12
f3 ~ c(a, b)*f2 + c(b, a)*f1
"
fit <- sem(mod, data_sem16, group = "group")

mod_nogp <-
"
f1 =~ x1 + a*x2 + x3 + b*x4
f2 =~ x5 + x6 + a*x7 + d*x8
f3 =~ x9 + x10 + x11 + x12
f3 ~ e*f2 + f*f1
b == d
ef := e*f
"
fit_nogp <- sem(mod_nogp, data_sem16)


test_that("Labelled parameters", {
  expect_no_error(fix_to_zero(fit, par_id = 2))
  expect_no_error(fix_to_zero(fit, par_id = 4))
  expect_no_error(fix_to_zero(fit, par_id = 8))
  expect_no_error(fix_to_zero(fit, par_id = 53))
  expect_no_error(fix_to_zero(fit_nogp, par_id = 2))
  expect_no_error(fix_to_zero(fit_nogp, par_id = 7))
  expect_no_error(fix_to_zero(fit_nogp, par_id = 8))
  expect_no_error(fix_to_zero(fit_nogp, par_id = 13))
  expect_no_error(fix_to_zero(fit_nogp, par_id = 14))
})

# Need to add tests for
# Free parameters involved in any constraints
# Free parameters already labelled
# Free parameters used in user-defined variable(s)

library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

mod1 <-
"
f1 =~ x1 + x2 + x3
f2 =~ x5 + x6
"
fit1 <- sem(mod1, data_sem16)

mod1b <-
"
f1 =~ x1 + x2 + x3
f2 =~ x5 + x6
f1 ~~ 0*f2
"
suppressWarnings(fit1b <- sem(mod1b, data_sem16))

test_that("Models not identified", {
  expect_error(fix_to_zero(fit1, par_id = 13))
})

# Need to add tests for
# Free parameters involved in any constraints
# Free parameters already labelled
# Free parameters used in user-defined variable(s)

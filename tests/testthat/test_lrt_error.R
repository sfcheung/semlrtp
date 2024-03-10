library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

mod <-
"
x1 ~ a1*x2 + a2*x3
x4 ~ b*x1
"
dat <- data_sem16[1:30, ]
dat$x2 <- dat$x2^3
dat$x1 <- dat$x1^(1 / -2)
dat$x4 <- dat$x4^(1 / -2)

fit <- sem(mod, dat, estimator = "MLR")

fit_i <- sem(c(mod, c("b == 0")), dat, estimator = "MLR")

test_that("Trap LRT error", {
  lrt_i <- lrt(fit, par_id = 3)
  expect_error(lrt_i_check <- lavTestLRT(fit_i, fit))
  expect_true(is.na(lrt_i$lrt))
  expect_true(lrt_i$lrt_status != 0)
  expect_true(inherits(lrt_i$lrt_msg, "error"))
})

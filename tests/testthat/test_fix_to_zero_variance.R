library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# If a variance is fixed, fix corresponding covariance

# cfa() example
data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
x3 ~~ x6
x3 ~~ x9
"
fit <- cfa(mod, data = data_sem16, group = "group")
fit_nogp <- cfa(mod, data = data_sem16)

fit_i <- fix_to_zero(fit, par_id = 85, store_fit = TRUE)
fit_i_nogp <- fix_to_zero(fit_nogp, par_id = 27, store_fit = TRUE)

test_that("Fix variance", {
  df0 <- ifelse(identical(NA, (fit_i$fit0)),
                lavInspect(fit_i$fit_not_ok, "npar"),
                lavInspect(fit_i$fit0, "npar"))
  expect_equal(lavInspect(fit_i$fit1, "npar") - df0,
               3,
               ignore_attr = TRUE)
  df0 <- ifelse(identical(NA, (fit_i_nogp$fit0)),
                lavInspect(fit_i_nogp$fit_not_ok, "npar"),
                lavInspect(fit_i_nogp$fit0, "npar"))
  expect_equal(lavInspect(fit_i_nogp$fit1, "npar") - df0,
               2,
               ignore_attr = TRUE)
})

# SEM

data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
f3 ~ f2 + f1
f4 ~ f2 + f1
"
fit_sem <- sem(mod, data_sem16, group = "group")
fit_sem_nogp <- sem(mod, data_sem16)

fit_i <- fix_to_zero(fit_sem, par_id = 101, store_fit = TRUE)
fit_i_nogp <- fix_to_zero(fit_sem_nogp, par_id = 40, store_fit = TRUE)

test_that("Fix variance", {
  df0 <- ifelse(identical(NA, (fit_i$fit0)),
                lavInspect(fit_i$fit_not_ok, "npar"),
                lavInspect(fit_i$fit0, "npar"))
  expect_equal(lavInspect(fit_i$fit1, "npar") - df0,
               2,
               ignore_attr = TRUE)
  df0 <- ifelse(identical(NA, (fit_i_nogp$fit0)),
                lavInspect(fit_i_nogp$fit_not_ok, "npar"),
                lavInspect(fit_i_nogp$fit0, "npar"))
  expect_equal(lavInspect(fit_i_nogp$fit1, "npar") - df0,
               2,
               ignore_attr = TRUE)
})


library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
HS.model <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + x6
              f3 =~ x7 + x8 + x9'
fit <- cfa(HS.model, data = dat, group = "school")
fit_nogp <- cfa(HS.model, data = dat)

test_that("lhs_one_par", {
  expect_equal(lhs_to_par_id(fit = fit, par = "f2 =~ x5", group = 2), 41)
  expect_equal(lhs_to_par_id(fit = fit, par = "f2 ~~   f3", group = "Pasteur"), 60)
  expect_equal(lhs_to_par_id(fit = fit, par = "x5 ~1", group = "Pasteur"), 65)
  expect_equal(lhs_to_par_id(fit = fit, par = "f3 ~~ f2", group = "Pasteur"), 60)
})

test_that("lhs_one_par: error", {
  expect_error(lhs_to_par_id(fit = fit, par = "f2 =~ x1", group = 2))
  expect_error(lhs_to_par_id(fit = fit, par = "f2 ~~   f3"))
  expect_error(lhs_to_par_id(fit = fit, par = "x5 ~1", group = "Test"))
  expect_error(lhs_to_par_id(fit = fit, par = c("f3 ~~ f2", "abc"), group = "Pasteur"))
})

## TO PROCESS

# SEM

set.seed(12345)
dat <- HolzingerSwineford1939[sample.int(301, 80), ]
mod_sem <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + x8 + x9
             f2 ~ f1
             f3 ~ f2'
fit_sem <- sem(mod_sem, data = dat, group = "school")
fit_sem_nogp <- cfa(mod_sem, data = dat)

# SEM with constraints

set.seed(12345)
dat <- HolzingerSwineford1939[sample.int(301, 80), ]
mod_sem_c <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + c(a, a)*x8 + x9
             f2 ~ f1
             f3 ~ f2'
fit_sem_c <- sem(mod_sem_c, data = dat, group = "school")
mod_sem_c_nogp <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + x8 + x9
             f2 ~ a*f1
             f3 ~ b*f2
             a == b'
fit_sem_c_nogp <- sem(mod_sem_c_nogp, data = dat)

# Test

set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
HS.model_i <- ' f1 =~ x1 + x2 + x3
                f2 =~ x4 + x5 + x6
                f3 =~ x7 + x8 + x9
                f1 ~~ c(0, NA)*f2
                f1 ~~ f3
                f2 ~~ f3'
fit_i <- cfa(HS.model_i, data = dat, group = "school")
lrt_i_check <- lavTestLRT(fit_i, fit)

lrt_i <- lrt(fit, par_id = 22)
test_that("CFA multiple group", {
  expect_equal(lrt_i$lrt[2, "Chisq diff"],
               lrt_i_check[2, "Chisq diff"])
})

# Test

# Do LRTp on one parameter

out <- lrt(fit, par_id = 22)
class(out)

library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
HS.model <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + x6
              f3 =~ x7 + x8 + x9'
fit_cfa <- cfa(HS.model, data = dat, group = "school", do.fit = FALSE)
fit_cfa_nogp <- cfa(HS.model, data = dat, do.fit = FALSE)

# SEM

set.seed(12345)
dat <- HolzingerSwineford1939[sample.int(301, 80), ]
mod_sem <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + x8 + x9
             f2 ~ f1
             f3 ~ f2'
fit_sem <- sem(mod_sem, data = dat, group = "school", do.fit = FALSE)
fit_sem_nogp <- cfa(mod_sem, data = dat, do.fit = FALSE)

# SEM with constraints

set.seed(12345)
dat <- HolzingerSwineford1939[sample.int(301, 80), ]
mod_sem_c <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + c(a, a)*x8 + x9
             f2 ~ f1
             f3 ~ f2'
fit_sem_c <- sem(mod_sem_c, data = dat, group = "school", do.fit = FALSE)
mod_sem_c_nogp <- ' f1 =~ x1 + x2 + x3
             f2 =~ x4 + x5 + x6
             f3 =~ x7 + x8 + x9
             f2 ~ a*f1
             f3 ~ b*f2
             a == b'
fit_sem_c_nogp <- sem(mod_sem_c_nogp, data = dat, do.fit = FALSE)
mod_sem_c_ecov <- ' f1 =~ x1 + x2 + x3
                    f2 =~ x4 + x5 + x6
                    f3 =~ x7 + c(a, a)*x8 + x9
                    f2 ~ f1
                    f3 ~ f2
                    x2 ~~ x5'
fit_sem_c_ecov <- sem(mod_sem_c_ecov, data = dat, group = "school", do.fit = FALSE)

# free_pars

test_that("cfa: default", {
  tmp <- free_pars(fit_cfa)
  ptable <- parameterTable(fit_cfa)
  expect_true(all(ptable[tmp, "free"] > 0))
  expect_true(all(ptable[tmp, "op"] == "~~"))
})

test_that("sem: op", {
  tmp <- free_pars(fit_sem)
  ptable <- parameterTable(fit_sem)
  expect_true(all(ptable[tmp, "free"] > 0))
  expect_true(all(ptable[tmp, "op"] == "~"))
  tmp <- free_pars(fit_sem, op = c("~1"))
  expect_true(all(ptable[tmp, "op"] == "~1"))
  tmp <- free_pars(fit_sem, op = c("~1", "=~"))
  expect_true(all(ptable[tmp, "op"] %in% c("=~", "~1")))
})

test_that("sem: variances", {
  tmp <- free_pars(fit_sem, no_variances = FALSE)
  ptable <- parameterTable(fit_sem)
  expect_true(all(ptable[tmp, "free"] > 0))
  expect_true(all(ptable[tmp, "op"] %in% c("~", "~~")))
  tmp2 <- which((ptable$lhs == ptable$rhs) &
                 ptable$lhs %in% c("f1"))
  expect_true(all(tmp2 %in% tmp))
})

test_that("sem: error_variances", {
  tmp <- free_pars(fit_sem, no_variances = FALSE, no_error_variances = FALSE)
  ptable <- parameterTable(fit_sem)
  tmp2 <- which((ptable$lhs == ptable$rhs) &
                 ptable$lhs %in% c("f1", "f2", "f3"))
  expect_true(all(tmp2 %in% tmp))
})

test_that("sem: error covariances", {
  tmp <- free_pars(fit_sem_c_ecov, no_error_covariances = FALSE)
  ptable <- parameterTable(fit_sem_c_ecov)
  expect_true(all(ptable[tmp, "free"] > 0))
  expect_true(all(ptable[tmp, "op"] %in% c("~", "~~")))
  tmp2 <- which((ptable$lhs != ptable$rhs) &
                (ptable$op == "~~"))
  expect_true(all(tmp2 %in% tmp))
})

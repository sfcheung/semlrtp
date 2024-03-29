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

fit_i <- fix_to_zero(fit, par_id = 22)
test_that("Factor covariances", {
  expect_true(is.na(parameterEstimates(fit_i$fit0)[22, "z"]))
})

# Need to add tests for
# Free parameters involved in any constraints
# Free parameters already labelled
# Free parameters used in user-defined variable(s)
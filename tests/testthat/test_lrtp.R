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

# Test

fit_lrtp <- lrtp(fit)
fit_lrtp_df <- as.data.frame(fit_lrtp)
test_that("CFA multiple group", {
  expect_true(is.numeric(fit_lrtp_df[c(22:24, 58:60), "LRTp"]))
  expect_true(all(is.na(fit_lrtp_df[-c(22:24, 58:60), "LRTp"])))
})

fit_lrtp_std <- lrtp(fit, standardized = TRUE)
test_that("CFA multiple group, std", {
  expect_true("std.all" %in% colnames(fit_lrtp_std))
})

# TODO:
# More tests
skip_on_cran()
# Slow tests

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

# Test

fit_lrtp <- lrtp(fit)
fit_lrtp_parallel <- lrtp(fit, parallel = TRUE, ncores = 2)
fit_lrtp_df <- as.data.frame(fit_lrtp)
fit_lrtp_parallel_df <- as.data.frame(fit_lrtp_parallel)
test_that("Parallel processing", {
  expect_equal(fit_lrtp_df$LRTp,
               fit_lrtp_parallel_df$LRTp,
               tolerance = 1e-4)
})

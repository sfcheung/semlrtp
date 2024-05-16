library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

dat <- data_sem16
set.seed(123456)
dat$x4 <- rnorm(nrow(dat))

mod <-
"
f1 =~ x1 + x2 + x3 + x4
"

fit <- sem(mod, dat)

mod0 <-
"
f1 =~ x1 + x2 + 0*x3 + x4
"

suppressWarnings(fit0 <- sem(mod0, dat))

skip_if_not(isFALSE(lavInspect(fit0, "converged")))

out <- fix_to_zero(fit, par_id = 3)
out_lrt <- lrt(fit, par_id = 3)
out_lrtp <- lrtp(fit, op = "=~")
out_lrtp_df <- as.data.frame(out_lrtp)

test_that("Not converged", {
  expect_true(is.na(out$fit0))
  expect_false(out$converged)
  expect_true(is.na(out_lrt$lrt))
  # Post.check failure is now acceptable
  expect_equal(out_lrtp_df[3, "LRT"],
               -1)
})

library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

dat <- data_sem16
set.seed(5165565)
dat$x4 <- dat$x1 + rnorm(nrow(dat), 0, 10)

mod <-
"
f1 =~ x1 + x2 + x3 + x4
"

suppressWarnings(fit <- sem(mod, dat))

mod0 <-
"
f1 =~ x1 + x2 + 0*x3 + x4
"

suppressWarnings(fit0 <- sem(mod0, dat))

skip_if_not(suppressWarnings(isFALSE(lavInspect(fit0, "post.check"))))

out2 <- fix_to_zero(fit, par_id = 2)
out2_lrt <- lrt(fit, par_id = 2)

out <- fix_to_zero(fit, par_id = 3)
out_lrt <- lrt(fit, par_id = 3)
out_lrtp <- lrtp(fit, op = "=~")
out_lrtp_df <- as.data.frame(out_lrtp)

test_that("Converged but Heywood", {
  # post.check is now acceptable
  expect_true(inherits(out$fit0, "lavaan"))
  expect_true(out$converged)
  expect_false(out$post_check_passed)
  # post.check is now acceptable
  expect_true(inherits(out_lrt$lrt, "anova"))
  # post.check is now acceptable
  expect_equal(out_lrtp_df[2:4, "LRT"],
               c(-1, 0, 0))
})

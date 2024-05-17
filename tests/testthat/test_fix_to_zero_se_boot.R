library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
dat <- HolzingerSwineford1939[1:200, ]
HS.model <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + x6'
suppressWarnings(fit <- cfa(HS.model, data = dat, se = "boot", bootstrap = 50, iseed = 2345))

HS.model.0 <- ' f1 =~ x1 + x2 + x3
                f2 =~ x4 + x5 + x6
                f1 ~~ 0*f2'
suppressWarnings(fit0 <- cfa(HS.model.0, data = dat, se = "standard", bootstrap = 50, iseed = 2345))

fit_diff <- lavTestLRT(fit, fit0)

# Should change se to "standard"

test_that("se = boot", {
  fit_i <- fix_to_zero(fit, par_id = 15)
  expect_equal(lavInspect(fit_i$fit0, "options")$se,
               "standard")
  fit_i_boot <- fix_to_zero(fit, par_id = 15, se_keep_bootstrap = TRUE)
  expect_equal(lavInspect(fit_i_boot$fit0, "options")$se,
               "bootstrap")
  lrt_i <- lrt(fit, par_id = 15)
  expect_equal(lavInspect(lrt_i$fix_to_zero$fit0, "options")$se,
               "standard")
  lrt_i_boot <- lrt(fit, par_id = 15, se_keep_bootstrap = TRUE)
  expect_equal(lavInspect(lrt_i_boot$fix_to_zero$fit0, "options")$se,
               "bootstrap")
  expect_output(print(lrt_i), "'standard'")
  expect_false(any(grepl("standard",
                         capture.output(print(lrt_i_boot)),
                         fixed = TRUE)))
  lrtp_i <- lrtp(fit)
  lrtp_i_boot <- lrtp(fit, se_keep_bootstrap = TRUE)
  expect_output(print(lrtp_i), "se_keep_bootstrap")
  expect_false(any(grepl("se_keep_bootstrap",
                capture.output(print(lrtp_i_boot)),
                fixed = TRUE)))
})


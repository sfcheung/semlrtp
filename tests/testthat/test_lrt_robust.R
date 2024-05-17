library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
# Find a subset that will lead to a warning in lavTestLRT
dat <- HolzingerSwineford1939[1:150 + 50, ]
HS.model <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + x6
              f3 =~ x7 + x8 + x9'
HS.model_i <- ' f1 =~ x1 + x2 + x3
                f2 =~ x4 + x5 + x6
                f3 =~ x7 + x8 + x9
                f1 ~~ c(0, NA)*f2
                f1 ~~ f3
                f2 ~~ f3'
fit <- cfa(HS.model, data = dat, group = "school", estimator = "MLR")
fit_nogp <- cfa(HS.model, data = dat, estimator = "MLM")

HS.model_i_nogp <- ' f1 =~ x1 + x2 + x3
                     f2 =~ x4 + x5 + x6
                     f3 =~ x7 + x8 + x9
                     f1 ~~ 0*f2
                     f1 ~~ f3
                     f2 ~~ f3'
fit_i_nogp <- cfa(HS.model_i_nogp, data = dat, estimator = "MLM")
fit_i <- cfa(HS.model_i, data = dat, group = "school", estimator = "MLR")

suppressWarnings(lrt_i_check <- lavTestLRT(fit_i, fit))
lrt_i_check_satorra2000 <- lavTestLRT(fit_i, fit, method = "satorra.2000")
lrt_i_nogp_check <- lavTestLRT(fit_i_nogp, fit_nogp)

# Test

test_that("LRT robust", {
  lrt_i <- lrt(fit, par_id = 22)
  lrt_i_nogp <- lrt(fit_nogp, par_id = 22)
  lrtp_i <- lrtp(fit)
  lrtp_i_nogp <- lrtp(fit_nogp)

  expect_equal(lrt_i$lrt[2, "Chisq diff"],
               lrt_i_check_satorra2000[2, "Chisq diff"])
  expect_equal(lrt_i_nogp$lrt[2, "Chisq diff"],
               lrt_i_nogp_check[2, "Chisq diff"],
               tolerance = 1e-4)
  expect_output(print(lrtp_i),
                "satorra.2000")
  expect_output(print(lrtp_i_nogp),
                "satorra.bentler.2001")

})

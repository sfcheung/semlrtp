skip_on_cran()
# A slow test

library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
mod <- ' f1 =~ x1 + x2 + x3
         f2 =~ x4 + x5 + x6
         f3 =~ x7 + x8 + x9
         f2 ~ c(a1, a2)*f1
         f3 ~ c(b1, b2)*f2
         ab1 := a1*b1
         ab2 := a2*b2'
fit <- sem(mod, data = dat, group = "school")
mod_nogp <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + x6
              f3 =~ x7 + x8 + x9
              f2 ~ a*f1
              f3 ~ b*f2
              ab := a*b'
fit_nogp <- sem(mod_nogp, data = dat)

# Test

mod_i <- ' f1 =~ x1 + x2 + x3
           f2 =~ x4 + x5 + x6
           f3 =~ x7 + x8 + x9
           f2 ~ c(a1, a2)*f1
           f3 ~ c(b1, b2)*f2
           ab1 := a1*b1
           ab2 := a2*b2
           ab1 == 0'
fit_i <- sem(mod_i, data = dat, group = "school")
lrt_i_check <- lavTestLRT(fit_i, fit)

lrt_i <- lrt(fit, par_id = 71)
test_that("SEM multiple group: User-parameter", {
  expect_equal(lrt_i$lrt[2, "Chisq diff"],
               lrt_i_check[2, "Chisq diff"],
               tolerance = 1e-4)
})

mod_nogp_i <- ' f1 =~ x1 + x2 + x3
                f2 =~ x4 + x5 + x6
                f3 =~ x7 + x8 + x9
                f2 ~ a*f1
                f3 ~ b*f2
                ab := a*b
                ab == 0'
fit_nogp_i <- sem(mod_nogp_i, data = dat)
lrt_nogp_i_check <- lavTestLRT(fit_nogp_i, fit_nogp)

lrt_nogp_i <- lrt(fit_nogp, par_id = 24)
test_that("SEM: User-parameter", {
  expect_equal(lrt_nogp_i$lrt[2, "Chisq diff"],
               lrt_nogp_i_check[2, "Chisq diff"],
               tolerance = 1e-4)
})

lrtp_i <- lrtp(fit, op = ":=")
test_that("lrtp: print", {
  expect_output(print(lrtp_i),
                format(round(lrt_i_check[2, "Pr(>Chisq)"], 3)))
})

lrtp_nogp_i <- lrtp(fit_nogp, op = ":=")
test_that("lrtp: print", {
  expect_output(print(lrtp_nogp_i),
                format(round(lrt_nogp_i_check[2, "Pr(>Chisq)"], 3)))
})



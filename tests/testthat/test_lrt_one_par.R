library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
HS.model <- ' f1 =~ x1 + x2 + x3
              f2 =~ x4 + x5 + c(d1, d3)*x6
              f3 =~ x7 + x8 + c(d1, d2)*x9
              f1 ~ c(a, a)*f2 + c(b, b)*f3'
HS.model.nogp <- ' f1 =~ x1 + x2 + x3
                   f2 =~ x4 + x5 + d1*x6
                   f3 =~ x7 + x8 + d1*x9
                   f1 ~ a*f2 + b*f3'
fit <- cfa(HS.model, data = dat, group = "school")
fit_nogp <- cfa(HS.model.nogp, data = dat)
pt <- parameterTable(fit)
pt_nogp <- parameterTable(fit_nogp)
chk1 <- pt[(pt$lhs == "f2") &
           (pt$op == "=~") &
           (pt$rhs == "x5") &
           (pt$group == 2), "id"]
chk2 <- pt[(pt$lhs == "f2") &
           (pt$op == "~~") &
           (pt$rhs == "f3") &
           (pt$group == 2), "id"]
chk3 <- pt[(pt$lhs == "x5") &
           (pt$op == "~1") &
           (pt$group == 1), "id"]
chk4 <- pt[pt$label == "d1", ][1, "id"]

chk21 <- pt_nogp[(pt_nogp$lhs == "f2") &
                 (pt_nogp$op == "=~") &
                 (pt_nogp$rhs == "x5"), "id"]
chk22 <- pt_nogp[(pt_nogp$lhs == "f2") &
                 (pt_nogp$op == "~~") &
                 (pt_nogp$rhs == "f3"), "id"]
chk23 <- pt_nogp[(pt_nogp$lhs == "x5") &
                 (pt_nogp$op == "~1"), "id"]

test_that("lhs_one_par", {
  expect_equal(lhs_to_par_id(fit = fit, par = "f2 =~ x5", group = 2), chk1)
  expect_equal(lhs_to_par_id(fit = fit, par = "f2 ~~   f3", group = "Pasteur"), chk2)
  expect_equal(lhs_to_par_id(fit = fit, par = "x5 ~1", group = "Grant-White"), chk3)
  expect_equal(lhs_to_par_id(fit = fit, par = "f3 ~~ f2", group = "Pasteur"), chk2)
  expect_equal(lhs_to_par_id(fit = fit, par = "d1"), chk4)
  expect_equal(lhs_to_par_id(fit = fit_nogp, par = "f2 =~ x5"), chk21)
  expect_equal(lhs_to_par_id(fit = fit_nogp, par = "f2 ~~   f3"), chk22)
  expect_equal(lhs_to_par_id(fit = fit_nogp, par = "f3 ~~ f2"), chk22)
})

test_that("lhs_one_par: error", {
  expect_error(lhs_to_par_id(fit = fit, par = "f2 =~ x1", group = 2))
  expect_error(lhs_to_par_id(fit = fit, par = "f2 ~~   f3"))
  expect_error(lhs_to_par_id(fit = fit, par = "x5 ~1", group = "Test"))
  expect_error(lhs_to_par_id(fit = fit, par = c("f3 ~~ f2", "abc"), group = "Pasteur"))
  expect_error(lhs_to_par_id(fit = fit_nogp, par = "x5 ~1"))
  expect_error(lhs_to_par_id(fit = fit, par = "f2 =~ x1"))
  expect_warning(lhs_to_par_id(fit = fit_nogp, par = "f3 ~~ f2", group = "Pasteur"))
})


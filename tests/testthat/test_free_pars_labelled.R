library(testthat)
library(semlrtp)
suppressMessages(library(lavaan))

# cfa() example
set.seed(1234)
dat <- HolzingerSwineford1939[sample.int(301, 200), ]
mod_labelled_constrained <-
"
x1 ~ c(a, a)*x2 + c(b, b)*x3 + x4
x5 ~ c(b, d)*x6
a == 0
b == 0
"
fit_sem <- sem(mod_labelled_constrained,
               data = dat,
               group = "school")

mod_labelled_constrained_nogp <-
"
x1 ~ a*x2 + b*x3 + d*x4
x5 ~ b*x6
a == 0
b == d
d == 0
"
fit_sem_nogp <- cfa(mod_labelled_constrained_nogp,
                    data = dat)

# free_pars

# Labelled but constrained to zero

test_that("sem: group", {
  # 1, 2, 4, 24, 25 are *not* free
  tmp <- free_pars(fit_sem)
  ptable <- parameterTable(fit_sem)
  tmpchk <- which(ptable$label %in% c("a", "b"))
  expect_false(any(tmpchk %in% tmp))
})

test_that("sem: no group", {
  # 1, 2, 3, 4 are *not* free
  tmp <- free_pars(fit_sem_nogp)
  ptable <- parameterTable(fit_sem_nogp)
  tmpchk <- which(ptable$label %in% c("a", "d", "b"))
  expect_false(any(tmpchk %in% tmp))
})

# Generate data
# usethis::use_data(testdata, overwrite = TRUE)

library(lavaan)
mod1 <-
"
f1 =~ .5*x1 + .5*x2 + .5*x3 + .5*x4
f2 =~ .5*x5 + .5*x6 + .5*x7 + .5*x8
f3 =~ .5*x9 + .5*x10 + .5*x11 + .5*x12
f4 =~ .5*x13 + .5*x14 + .5*x15 + .5*x16
f2 ~ .3*f1
f3 ~ .4*f1 + .5*f2
f4 ~ .6*f3
"
mod2 <-
"
f1 =~ .5*x1 + .5*x2 + .5*x3 + .5*x4
f2 =~ .5*x5 + .5*x6 + .5*x7 + .0*x8
f3 =~ .5*x9 + .5*x10 + .5*x11 + .5*x12
f4 =~ .5*x13 + .5*x14 + .0*x15 + .5*x16
f2 ~ .3*f1
f3 ~ .4*f1 + .0*f2
f4 ~ .6*f3 + .2*f1
"
dat1 <- simulateData(mod1, sample.nobs = 16 * 10, seed = 12345)
dat2 <- simulateData(mod2, sample.nobs = 16 * 11, seed = 67890)
dat1$group <- "gamma"
dat2$group <- "alpha"
dat <- rbind(dat1, dat2)
data_sem16 <- dat
usethis::use_data(data_sem16, overwrite = TRUE)

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lrtp.R
\name{lrtp}
\alias{lrtp}
\title{Likelihood Ratio Test P-Values}
\usage{
lrtp(
  fit,
  op = c("~", "~~"),
  no_variances = TRUE,
  no_error_variances = TRUE,
  no_error_covariances = TRUE,
  ...
)
}
\arguments{
\item{fit}{A \code{lavaan}-class object.}

\item{op}{A character vector of
\code{lavaan} model syntax operators.
Free parameters of these operators
will be included, unless excluded
by other arguments. Default is
\code{c("~", "~~")}.}

\item{no_variances}{Logical. If
\code{TRUE}, the default, then all
free variances, including error
variances, are excluded.}

\item{no_error_variances}{Logical,
If \code{TRUE}, the default, then all
free error variances are excluded.}

\item{no_error_covariances}{Logical.
If \code{TRUE}, the default, then all
free error covariances are excluded.}

\item{...}{Optional arguments to be
passed to \code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}}.}
}
\value{
An \code{lrt}-class object, which is a
data-frame-like object with the
output of \code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}},
with a column \code{LRTp} for the LRT
\emph{p}-values. Has a print method
\code{\link[=print.lrtp]{print.lrtp()}}.
}
\description{
Compute the likelihood ratio
test \emph{p}-values for free parameters in
a lavaan output.
}
\details{
It find free parameters in a
\code{lavaan}-class object, compute the
likelihood ratio test (LRT) \emph{p}-value
for each of them when fixed to zero,
and return a parameter estimates data
frame with the LRT \emph{p}-values
included.
}
\examples{

library(lavaan)
data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f4 =~ x10 + x11 + x12
f2 ~~ f1
f3 ~ f1 + f2
f4 ~ f3
"
fit <- sem(mod, data_sem16)
lrtp(fit)
lrtp(fit, op = "~")

}
\seealso{
\code{\link[=print.lrtp]{print.lrtp()}}
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}

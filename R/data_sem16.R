#' @title Sample Dataset For Test (16 Items and 2 Groups)
#'
#' @description A 16-variable dataset with 336 cases.
#'
#' @format A data frame with 336 rows
#' and 16 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#'   \item{x7}{Indicator. Numeric.}
#'   \item{x8}{Indicator. Numeric.}
#'   \item{x9}{Indicator. Numeric.}
#'   \item{x10}{Indicator. Numeric.}
#'   \item{x11}{Indicator. Numeric.}
#'   \item{x12}{Indicator. Numeric.}
#'   \item{x13}{Indicator. Numeric.}
#'   \item{x14}{Indicator. Numeric.}
#'   \item{x15}{Indicator. Numeric.}
#'   \item{x16}{Indicator. Numeric.}
#'   \item{group}{Group with two values, "alpha" and "gamma". Character.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_sem16)
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3 + x4
#' f2 =~ x5 + x6 + x7 + x8
#' f3 =~ x9 + x10 + x11 + x12
#' f4 =~ x13 + x14 + x15 + x16
#' f3 ~ f2 + f1
#' f4 ~ f3
#' "
#' fit <- sem(mod, data_sem16)
#' summary(fit)
"data_sem16"

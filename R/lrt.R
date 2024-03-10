#' @title Fix a Free Parameter to Zero and do LR Test
#'
#' @description Fix the designated
#' free parameter to zero and do a
#' likelihood ratio test.
#'
#' @details It fixes the designated
#' free parameter in a `lavaan` output,
#' refit the model, and do a likelihood
#' ratio test comparing this model with
#' the original model.
#'
#' @return
#' A `lrt`-class object, which is a
#' list with the following elements:
#'
#' - `lrt`: The output of [lavaan::lavTestLRT()].
#' If there is an error message or
#' warning, it is set to `NA`.
#'
#' - `par_id`: The row number of the
#' designated free parameters.
#'
#' - `fit0`: The `lavaan` output of the
#' modified model, with the designated
#' free parameter fixed to zero.
#'
#' - `fit1`: The original `lavaan`
#' output.
#'
#' - `call`: The call to this function.
#'
#' - `lrt_status`: Integer. If 0, then
#' there is no error nor warning
#' in the likelihood ratio test and
#' [lavaan::lavTestLRT()] returns a
#' table (`data.frame`) of the test.
#' If -1, then something is wrong,
#' e.g., an error or warning occurred.
#'
#' - `lrt_msg`: If something is wrong
#' when doing the likelihood ratio
#' test, this is the error or warning
#' message. If no error nor warning,
#' this is `NA`.
#'
#' @param fit A `lavaan`-class object.
#'
#' @param par_id An integer. The row
#' number of the free parameter in the
#' parameter table of `fit` to be
#' fixed.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' library(lavaan)
#' data(data_sem16)
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' "
#' fit <- sem(mod, data_sem16)
#' # Fix the factor covariance to zero
#' out <- lrt(fit, par_id = 15)
#' out$lrt
#' parameterEstimates(fit)[15, ]
#' parameterEstimates(out$fit0)[15, ]
#'
#' @export

lrt <- function(fit,
                par_id) {
    if (isFALSE(inherits(fit, "lavaan"))) {
        stop("The fit object is not a lavaan object.")
      }
    fit_i <- fix_to_zero(fit,
                         par_id = par_id)
    fit0 <- fit_i$fit0
    lrt_out <- NA
    lrt_msg <- NA
    if (inherits(fit0, "lavaan")) {
        lrt_msg <- tryCatch(lrt_out <- lavaan::lavTestLRT(fit,
                                                          fit0),
                               error = function(e) e,
                               warning = function(w) w)
      }
    lrt_status <- c(NotOK = -1)
    if (inherits(lrt_out, "anova")) {
        if (inherits(lrt_msg, "error") ||
            inherits(lrt_msg, "warning")) {
              lrt_out <- NA
              lrt_status <- c(NotOK = -1)
            } else {
              lrt_status <- c(OK = 0)
            }
      } else {
        lrt_out <- NA
      }
    out <- list(lrt = lrt_out,
                par_id = par_id,
                fit0 = fit0,
                fit1 = fit,
                call = match.call(),
                lrt_status = lrt_status,
                lrt_msg = lrt_msg)
    class(out) <- c("lrt", class(out))
    out
  }
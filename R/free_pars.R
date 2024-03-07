#' @title Identify Free Parameters
#'
#' @description Identify free parameters
#' in a lavaan object.
#'
#' @details This function identifies
#' the free parameters that meets user
#' requirements.
#'
#' @return
#' A numeric vector of the row number(s)
#' in the parameter table of the
#' lavaan object
#'
#' @param fit A `lavaan`-class object.
#'
#' @param op A character vector of
#' `lavaan` model syntax operators.
#' Free parameters of these operators
#' will be included, unless excluded
#' by other arguments. Default is
#' `c("~", "~~")`.
#'
#' @param no_variances. Logical. If
#' `TRUE`, the default, then all
#' free variances, including error
#' variances, are excluded.
#'
#' @param no_error_variances. Logical,
#' If `TRUE`, the default, then all
#' free error variances are excluded.
#'
#' @param no_error_covariances. Logical.
#' If `TRUE`, the default, then all
#' free error covariances are excluded.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' data(data_sem16)
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' "
#' fit <- sem(mod, data_sem16)
#' free_pars(fit)
#' free_pars(fit, op = "=~")
#' free_pars(fit, op = "~")
#'
#' @noRd

free_pars <- function(fit,
                      op = c("~", "~~"),
                      no_variances = TRUE,
                      no_error_variances = TRUE,
                      no_error_covariances = TRUE) {
    if (isFALSE(inherits(fit, "lavaan"))) {
        stop("The fit object is not a lavaan object.")
      }
    if (no_variances) {
        no_error_variances <- TRUE
      }
    ptable <- lavaan::parameterTable(fit)
    q <- nrow(ptable)
    ids <- seq_len(q)
    ids_op <- which(ptable$op %in% op)
    ids_variances <- (ptable$lhs == ptable$rhs) & ptable$op == "~~"
    ids_variances <- which(ids_variances)
    dvs <- unique(c(lavaan::lavNames(fit, "ov.ind"),
                    lavaan::lavNames(fit, "ov.nox"),
                    lavaan::lavNames(fit, "lv.nox")))
    ids_error_variances <- (ptable$op == "~~") &
                           (ptable$lhs %in% dvs) &
                           (ptable$rhs %in% dvs) &
                           (ptable$lhs == ptable$rhs)
    ids_error_variances <- which(ids_error_variances)
    ids_error_covariances <- (ptable$op == "~~") &
                             (ptable$lhs %in% dvs) &
                             (ptable$rhs %in% dvs) &
                             (ptable$lhs != ptable$rhs)
    ids_error_covariances <- which(ids_error_covariances)
    i <- intersect(which((ptable$free > 0)), ids_op)
    if (no_variances) {
        i <- setdiff(i, ids_variances)
      }
    if (no_error_variances) {
        i <- setdiff(i, ids_error_variances)
      }
    if (no_error_covariances) {
        i <- setdiff(i, ids_error_covariances)
      }
    ids_final <- ids[i]
    ids_final
  }
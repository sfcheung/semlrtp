#' @title Likelihood Ratio Test P-Values
#'
#' @description Compute the likelihood ratio
#' test (LRT) *p*-values for free
#' parameters in
#' a `lavaan` output.
#'
#' @details It finds free parameters in a
#' `lavaan`-class object, computes the
#' likelihood ratio test (LRT) *p*-value
#' for each of them when fixed to zero,
#' and returns a parameter estimates table
#' with the LRT *p*-values
#' included.
#'
#' By default, it only computes LRT
#' *p*-values for regression paths
#' and covariances, except for
#' error covariances. This default
#' can be overridden using arguments
#' such as `op`, `no_variances`,
#' `no_error_variances`,
#' and `no_error_covariances`.
#'
#' ## Technical Details
#'
#' It first identify the parameters
#' to be processed, and then call
#' [lrt()] on each of them.
#' Please refer to
#' <https://sfcheung.github.io/semlrtp/articles/internal_workflow.html>
#' for the internal workflow.
#'
#' @return
#' An `lrt`-class object, which is a
#' data-frame-like object similar to the
#' output of [lavaan::parameterEstimates()],
#' with a column `LRTp` for the LRT
#' *p*-values, as well as other columns
#' such as the chi-square difference in
#' the test. it has a print method,
#' [print.lrtp()].
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
#' @param no_variances Logical. If
#' `TRUE`, the default, then all
#' free variances are excluded.
#' (Error variances are handled
#'  by `no_error_variances`.)
#'
#' @param no_error_variances Logical,
#' If `TRUE`, the default, then all
#' free error variances are excluded.
#'
#' @param no_error_covariances Logical.
#' If `TRUE`, the default, then all
#' free error covariances are excluded.
#'
#' @param ... Optional arguments to be
#' passed to [lavaan::parameterEstimates()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [print.lrtp()]
#'
#' @examples
#'
#' library(lavaan)
#' data(data_sem16)
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' f3 =~ x7 + x8 + x9
#' f4 =~ x10 + x11 + x12
#' f2 ~~ f1
#' f3 ~ f1 + f2
#' f4 ~ f3
#' "
#' fit <- sem(mod, data_sem16)
#' lrtp(fit)
#' lrtp(fit, op = "~")
#'
#' @export

lrtp <- function(fit,
                 op = c("~", "~~"),
                 no_variances = TRUE,
                 no_error_variances = TRUE,
                 no_error_covariances = TRUE,
                 ...) {
    ids <- free_pars(fit = fit,
                     op = op,
                     no_variances = no_variances,
                     no_error_variances = no_error_variances,
                     no_error_covariances = no_error_covariances)
    if (length(ids) > 0) {
        lrt_out <- lapply(ids, lrt,
                          fit = fit)
        names(lrt_out) <- ids
      } else {
        lrt_out <- NULL
      }
    # Find a way to store group labels
    args <- list(...)
    args <- utils::modifyList(args,
                              list(object = fit,
                                   header = TRUE,
                                   output = "text"))
    out0 <- do.call(lavaan::parameterEstimates,
                    args = args)
    ptable <- lavaan::parameterTable(fit)
    match_id <- match_ptable_est(ptable = ptable,
                                 est = ptable)
    tmp0 <- colnames(out0)
    tmp4 <- match(c("est", "se", "z", "pvalue"), tmp0)
    if (all(is.na(tmp4))) {
        tmp1 <- which(colnames(out0) == "exo")
      } else {
        tmp4 <- tmp4[!is.na(tmp4)]
        tmp1 <- tmp4[length(tmp4)]
      }
    tmp2 <- ncol(out0)
    if (tmp2 == tmp1) {
        out <- cbind(out0,
                     LRT = NA,
                     Chisq = NA,
                     LRTp = NA)
      } else {
        out <- cbind(out0[, 1:tmp1],
                     LRT = NA,
                     Chisq = NA,
                     LRTp = NA,
                     out0[, (tmp1 + 1):tmp2])
      }
    class(out) <- class(out0)
    if (length(ids) > 0) {
        ids_out <- as.numeric(names(lrt_out))
        est_id <- match_id$est_id[match(ids_out, match_id$id)]
        lrt_status <- sapply(lrt_out, `[[`, "lrt_status")
        lrt_raw <- sapply(lrt_out, `[[`, "lrt")
        lrt_chisqs <- sapply(lrt_out,
                             function(x) {
                                 if (inherits(x$lrt, "anova")) {
                                     return(unname(x$lrt[2, "Chisq diff"]))
                                   } else {
                                     return(NA)
                                   }
                               })
        lrt_pvalues <- sapply(lrt_out,
                              function(x) {
                                 if (inherits(x$lrt, "anova")) {
                                     return(unname(x$lrt[2, "Pr(>Chisq)"]))
                                   } else {
                                     return(NA)
                                   }
                                })
        out[est_id, "Chisq"] <- lrt_chisqs
        out[est_id, "LRTp"] <- lrt_pvalues
        out[est_id, "LRT"] <- lrt_status
        attr(out, "lrt") <- lrt_out
      }
    attr(out, "call") <- match.call
    fit_summary <- lavaan::summary(fit)
    out <- copy_est_attributes(target0 = out,
                               source0 = fit_summary$pe)
    class(out) <- c("lrtp", class(out))
    out
  }

#' @noRd

match_ptable_est <- function(ptable,
                             est) {
    if (max(ptable$group) == 1) {
        est$group <- 1
      }
    est$est_id <- seq_len(nrow(est))
    id_out <- merge(ptable[, c("lhs", "op", "rhs", "group", "id")],
                    est[, c("lhs", "op", "rhs", "group", "est_id")])
    id_out <- id_out[order(id_out$est_id), ]
    id_out
  }

#' @noRd

copy_est_attributes <- function(target0,
                                source0) {
    x <- names(attributes(source0))
    x <- setdiff(x, c("names", "row.names", "class"))
    for (y in x) {
        attr(target0, y) <- attr(source0, y)
      }
    target0
  }
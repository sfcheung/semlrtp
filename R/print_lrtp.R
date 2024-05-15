#' @title Print an 'lrtp'- Class Object
#'
#' @description Print the content of
#' an `lrtp`-class object.
#'
#' @details The print method for the
#' output of [lrtp()].
#'
#' @return
#' `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x An `lrtp`-class object.
#'
#' @param digits Integer. The number of
#' decimal places to print. Default is
#' 3.
#'
#' @param lrtp_only Logical. If `TRUE`,
#' the default, only parameters with
#' LRT *p*-values will be printed.
#'
#' @param wald_stats Logical. If `TRUE`,
#' the usual Wald statistics (e.g.,
#' z statistics,
#' *p*-values, CIs) are printed. `FALSE`
#' by default, assuming that users
#' prefer using LRT statistics when
#' using [lrtp()].
#'
#' @param output The format of the
#' printout. If `"text"`, then the
#' style in the [summary()]
#' of the `lavaan`-class object is used.
#' If `"data.frame"`, then the data
#' frame format of [lavaan::parameterEstimates()]
#' is used.
#'
#' @param ...  Optional arguments. Not
#' used.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lrtp()]
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
#'
#' out <- lrtp(fit)
#' out
#' print(out, lrtp_only = FALSE)
#'
#' @export

print.lrtp <- function(x,
                       digits = 3,
                       lrtp_only = TRUE,
                       wald_stats = FALSE,
                       output = c("text", "data.frame"),
                       ...) {
    output <- match.arg(output)
    out <- x
    tmp <- class(out)
    class(out) <- class(out)[-which(tmp == "lrtp")]
    did_LRT <- !is.na(out$LRT)
    LRTstatus <- out$LRT
    LRTNotOK <- which(LRTstatus != 0)
    LRTp0 <- out$LRTp
    Chisq0 <- out$Chisq
    LRTp0[LRTNotOK] <- -9
    Chisq0[LRTNotOK] <- -9
    LRTp_NA <- is.na(LRTp0)
    if (!any(did_LRT)) {
        LRTptxt <- rep("", length(LRTp0))
        Chisqtxt <- rep("", length(Chisq0))
      } else {
        LRTptxt <- formatC(LRTp0,
                          digits = digits,
                          format = "f",
                          width = max(8, digits + 2))
        Chisqtxt <- formatC(Chisq0,
                            digits = digits,
                            format = "f",
                            width = max(8, digits + 2))
      }
    tmp0 <- "Failed"
    tmp1 <- max(sapply(LRTptxt, nchar))
    tmp2 <- tmp1 - nchar(tmp0)
    if (tmp2 > 0) {
        tmp3 <- paste0(paste0(rep(" ", tmp2), collapse = ""), "Failed")
      } else {
        tmp3 <- tmp0
      }
    LRTptxt[LRTNotOK] <- tmp3
    tmp1 <- max(sapply(Chisqtxt, nchar))
    tmp2 <- tmp1 - nchar(tmp0)
    if (tmp2 > 0) {
        tmp3 <- paste0(paste0(rep(" ", tmp2), collapse = ""), "Failed")
      } else {
        tmp3 <- tmp0
      }
    Chisqtxt[LRTNotOK] <- tmp3
    out1 <- out
    out1$LRTp <- LRTptxt
    out1$Chisq <- Chisqtxt
    out1$LRT <- NULL
    if (lrtp_only) {
        out1 <- out1[did_LRT, ]
      }
    if (!wald_stats) {
        out1$z <- NULL
        out1$pvalue <- NULL
        out1$ci.lower <- NULL
        out1$ci.upper <- NULL
      }
    if (output == "data.frame") {
        out1 <- data.frame(out1)
        class(out1) <- c("lavaan.data.frame",
                        class(out1))
      }
    pout <- utils::capture.output(print(out1,
                                        nd = digits))
    cat(pout, sep = "\n")
    if (!any(did_LRT)) {
        cat(strwrap("NOTE: No free parameters have LRT p-values.\n"),
            fill = TRUE)
      } else {
        if (length(LRTNotOK) > 0) {
        tmp <- paste("NOTE: 'Failed' indicates that LRT p-value is",
                     "requested but the likelihood ratio test failed.\n")
        cat(strwrap(tmp),
            fill = TRUE)
        }
      }
    invisible(x)
  }
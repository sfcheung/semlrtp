#' @title Print a `lrtp`- Class Object
#'
#' @description Print the content of
#' a `lrtp`-class object.
#'
#' @details The print method for the
#' output of [lrtp()].
#'
#' @return
#' `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x A `lrtp`-class object.
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
#' prefer using LRT statistics.
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
    LRTp0 <- out$LRTp
    LRTp_NA <- is.na(LRTp0)
    if (all(LRTp_NA)) {
        LRTptxt <- rep("", length(LRTp0))
      } else {
        LRTptxt <- formatC(LRTp0,
                          digits = digits,
                          format = "f",
                          width = max(8, digits + 2))
      }
    LRTptxt[LRTp_NA] <- ""
    out1 <- out
    out1$LRTp <- LRTptxt
    if (lrtp_only) {
        out1 <- out1[!LRTp_NA, ]
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
    if (all(LRTp_NA)) {
        cat("NOTE: No free parameters have LRT p-values.\n")
      }
    invisible(x)
  }
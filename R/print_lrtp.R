#' @title Print an 'lrtp'- Class Object
#'
#' @description Print the content of
#' an `lrtp`-class object.
#'
#' @details The print method for the
#' output of [lrtp()].
#'
#' Additional diagnostic information
#' will be printed if one or more
#' likelihood tests encounter some
#' errors or warnings.
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
#' If `"data.frame"` or `"table"`, then the data
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
                       output = c("text", "data.frame", "table"),
                       ...) {
    output <- match.arg(output)
    out <- x
    tmp <- class(out)
    class(out) <- class(out)[-which(tmp == "lrtp")]
    did_LRT <- !is.na(out$LRT)
    user_only <- identical(unique(out$op[did_LRT]), ":=")
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

    fit0_ok_s <- ifelse(out1$fit0_ok,
                        "",
                        "f")
    fit0_ok_s[is.na(fit0_ok_s)] <- ""
    out1$fit0_ok <- NULL

    converge_ok_s <- ifelse(out1$converge_ok,
                            "",
                            "c")
    converge_ok_s[is.na(converge_ok_s)] <- ""
    out1$converge_ok <- NULL

    vcov_ok_s <- ifelse(out1$vcov_ok,
                        "",
                        "v")
    vcov_ok_s[is.na(vcov_ok_s)] <- ""
    out1$vcov_ok <- NULL

    LRT_ok_s <- ifelse(out1$LRT_ok,
                       "",
                       "L")
    LRT_ok_s[is.na(LRT_ok_s)] <- ""
    out1$LRT_ok <- NULL

    pc_ok_s <- ifelse(out1$post_check_ok,
                       "",
                       "p")
    pc_ok_s[is.na(pc_ok_s)] <- ""
    out1$post_check_ok <- NULL

    LRTCode <- paste0(fit0_ok_s,
                        converge_ok_s,
                        vcov_ok_s,
                        LRT_ok_s,
                        pc_ok_s)

    if (any(LRTCode != "")) {
        out1$LRTCode <- LRTCode
      } else {
        out1$LRT_id <- NULL
      }
    if (lrtp_only && !user_only) {
        # Delete rows later if user_only
        out1 <- out1[did_LRT, ]
      }
    if (!wald_stats) {
        out1$z <- NULL
        out1$pvalue <- NULL
        out1$ci.lower <- NULL
        out1$ci.upper <- NULL
      }

    if (output %in% c("data.frame", "table")) {
        out1 <- data.frame(out1)
        class(out1) <- c("lavaan.data.frame",
                        class(out1))
      }
    pout <- utils::capture.output(print(out1,
                                        nd = digits))
    # Delete rows here if user_only
    if (user_only) {
        tmp1 <- which(grepl("Group 1", pout, fixed = TRUE))
        tmp2 <- which(grepl("Latent", pout, fixed = TRUE))
        tmp3 <- which(grepl("Regression", pout, fixed = TRUE))
        tmp4 <- which(grepl("Variance", pout, fixed = TRUE))
        tmp5 <- which(grepl("Intercept", pout, fixed = TRUE))
        tmp6 <- which(grepl("Threshold", pout, fixed = TRUE))
        tmpx <- which(grepl("Defined Parameters:", pout, fixed = TRUE))
        tmp0 <- sort(c(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))[1]
        pout <- pout[-(tmp0:(tmpx - 1))]
      }
    cat(pout, sep = "\n")
    if (!any(did_LRT)) {
        cat(strwrap("NOTE: No free parameters have LRT p-values.\n"),
            fill = TRUE)
      } else {
        if (!is.null(out1$LRTCode)) {
            cat("NOTE:\n")
            tmp <- paste("- 'Failed' indicates that LRT p-value is",
                        "requested but the likelihood ratio test failed.")
            cat(strwrap(tmp,
                        exdent = 2),
                sep = "\n")
            cat("- Interpreting the LRTCode:\n")
            if (any(grepl("f", out1$LRTCode))) {
                tmp <- paste("  - f: Error in fitting the restricted model.")
                cat(strwrap(tmp,
                            indent = 2,
                            exdent = 4),
                    sep = "\n")
              }
            if (any(grepl("c", out1$LRTCode))) {
                tmp <- paste("  - c: The restricted model did not converge.")
                cat(strwrap(tmp,
                            indent = 2,
                            exdent = 4),
                    sep = "\n")
              }
            if (any(grepl("v", out1$LRTCode))) {
                tmp <- paste("  - v: Problems in computing the variance-covariance",
                             "matrix of the parameters. The restricted",
                             "model may not be identified.")
                cat(strwrap(tmp,
                            indent = 2,
                            exdent = 4),
                    sep = "\n")
              }
            if (any(grepl("L", out1$LRTCode))) {
                tmp <- paste("  - L: Error in doing the",
                             "likelihood ratio test.")
                cat(strwrap(tmp,
                            indent = 2,
                            exdent = 4),
                    sep = "\n")
              }
            pc_failed <- any(grepl("p", out1$LRTCode))
            if (pc_failed) {
                tmp <- paste("  - p: The restricted model failed the",
                             "post.check of lavaan.")
                cat(strwrap(tmp,
                            indent = 2,
                            exdent = 4),
                    sep = "\n")
              }
            if (pc_failed) {
                tmp <- paste("- Note that if there are no other warnings or",
                             "errors, the likelihood ratio test may still",
                             "be acceptable even if failing the post.check.",
                             "Heywood case",
                             "does not necessarily mean model misspecification.",
                             "Nevertheless, it is still advised to check the",
                             "fit results.")
                tmp <- strwrap(tmp,
                               exdent = 2)
                cat(tmp, sep = "\n")
              }
            tmp <- paste("- The detail of a test can be examined by",
                         "attr(x, 'lrt')$`j`, j being the number in",
                         "the column LRT_id, and x the name of this",
                         "object. Typing attr(x, 'lrt') prints",
                         "all the tests.")
            cat(strwrap(tmp,
                        exdent = 2),
                sep = "\n")
          }
        lrt_out <- attr(out, "lrt")
        se_force_standard_vector <- sapply(lrt_out,
            function(x) x$fix_to_zero$se_force_standard)
        if (any(se_force_standard_vector)) {
            tmp <- paste("- Bootstrapping standard errors were requested",
                        "in the original model but was changed to",
                        "'standard' when fitting the restricted model.",
                        "Set se_keep_bootstrap to TRUE to keep using",
                        "bootstrapping standard errors but note that",
                        "this can take a long time to run.")
            tmp <- strwrap(tmp,
                          exdent = 2)
            cat(tmp, sep = "\n")
          }
        lrt_heading <- sapply(lrt_out,
            function(x) tryCatch(attr(x$lrt, "heading")))
        s2000 <- ifelse(any(grepl("satorra.2000", lrt_heading, fixed = TRUE)),
                        "satorra.2000",
                        NA)
        sb2001 <- ifelse(any(grepl("satorra.bentler.2001", lrt_heading, fixed = TRUE)),
                         "satorra.bentler.2001",
                         NA)
        sb2010 <- ifelse(any(grepl("satorra.bentler.2010", lrt_heading, fixed = TRUE)),
                         "satorra.bentler.2010",
                         NA)
        if (any(is.character(s2000),
                is.character(sb2001),
                is.character(sb2010), na.rm = TRUE)) {
            tmp <- c(s2000, sb2001, sb2010)
            tmp <- tmp[!is.na(tmp)]
            len1 <- length(tmp) == 1
            tmp <- paste0("- ",
                          ifelse(len1,
                                 "This method was",
                                 "THese methods were"),
                          " used in one or more of the likelihood ratio test(s): ",
                          paste(tmp, collapse = ", "))
            tmp <- strwrap(tmp,
                           exdent = 2)
            cat(tmp, sep = "\n")
          }
      }
    invisible(x)
  }
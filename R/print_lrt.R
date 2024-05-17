#' @title Print an 'lrt'- Class Object
#'
#' @description Print the content of
#' an `lrt`-class object.
#'
#' @details It is the print method for
#' the output of [lrt()].
#'
#' @return
#' `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x An `lrt`-class object.
#'
#' @param digits Integer. The number of
#' decimal places to print. Default is
#' 3.
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
#' out <- lrt(fit, par_id = "f1 ~~ f2")
#' out
#'
#' @export

print.lrt <- function(x,
                      digits = 3,
                      ...) {
    out <- x
    par_label <-  out$par_label
    lrt_status <- out$lrt_status

    cat("\n==== LRT p-value ====\n")
    cat("\n")
    cat("Parameter:", par_label, "\n")
    cat("\n")
    if (!isTRUE(lrt_status == 0)) {
        cat("Likelihood ratio test failed.",
            "Please check the parameter selected.",
            "\n")
        cat("lrt() status code:", out$lrt_status, "\n")
        cat("\n")
        cat("Results in fixing", par_label, "to zero:\n")
        cat("\n")
        cat("- Error message:", ifelse(is.na(out$fix_to_zero$fit0_error),
                                     "Nil\n",
                                     paste0("\n", as.character(out$fix_to_zero$fit0_error))))
        cat("- VCOV of parameters:")
        if (isTRUE(out$fix_to_zero$vcov_ok)) {
            cat(" OK\n")
          } else {
            cat("\n")
            cat(strwrap(as.character(out$fix_to_zero$vcov_msg),
                        indent = 2,
                        exdent = 2), sep = "\n")
          }
        cat("- Estimation convergence:", ifelse(isTRUE(out$fix_to_zero$converged),
                                            "Converged\n",
                                            "Failed to converge\n"))
        cat("- Solution post.check:", ifelse(isTRUE(out$fix_to_zero$post_check_passed),
                                           "Passed\n",
                                           "Failed\n"))
      } else {
        cat("LRT test with the selected parameter fixed to zero:\n")

        print(out$lrt, digits = digits)
        cat("\n")
      }

    # LRT result is accepted even if post.check failed
    if (isFALSE(out$fix_to_zero$post_check_passed)) {
        cat("- Post check warning:\n")
        tmp2 <- paste(as.character(unlist(out$fix_to_zero$post_check_msg)),
                      collapse = ",")
        tmp2 <- strwrap(tmp2,
                        indent = 2,
                        exdent = 2)
        cat(tmp2, sep = "\n")
        if (isTRUE(lrt_status == 0)) {
            tmp2 <- paste("- Note that if there are no other warnings or",
                          "errors, this likelihood ratio test may still",
                          "be acceptable because the Heywood case",
                          "does not necessarily mean model misspecification.",
                          "Nevertheless, it is still advised to check the",
                          "fit results, which can be inspected by",
                          "'summary(x$fix_to_zero$fit0)',",
                          "x being the name of this object.")
            tmp2 <- strwrap(tmp2,
                            exdent = 2)
            cat(tmp2, sep = "\n")
          }
      }
    if (out$fix_to_zero$se_force_standard) {
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

    invisible(x)
  }
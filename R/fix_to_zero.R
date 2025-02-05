#' @title Fix a Free Parameter To Zero And Fit a Model Again
#'
#' @description It fixes a designated free parameter
#' in a lavaan object to zero and refit the model.
#'
#' @details It modifies the parameter
#' table of a `lavaan`-class object and
#' then fits the model again.
#'
#' Users should usually call
#' [lrtp()] directly instead of calling
#' this function. It is exported for
#' developers.
#'
#' @return
#' A `fix_to_zero`-class object, which
#' is a list with these elements:
#'
#' - `fit0` is the `lavaan` output of the
#' refitted object. `NA` if the fit
#' failed for some reasons. To be
#' considered an acceptable solution,
#' the optimization must converge,
#' the solution passes `lavaan`'s
#' post check,
#' the variance-covariance matrix of
#' estimates successfully computed,
#' and the increase in the model
#' degree of freedom equal to the
#' expected change.
#'
#' - `fit1` is the original `lavaan`
#' output if `store_fit` is `TRUE`. It
#' is `NULL` if `store_fit` is `FALSE`,
#' the default.
#'
#' - `par_id` is the row number of the
#' designated free parameter in the
#' parameter table.
#'
#' - `call` is the original call to this
#' function.
#'
#' - `ptable0` is the parameter table
#' with the designated parameter fixed
#' to zero. It can be used for diagnostic
#' purpose if the fit failed.
#'
#' - `fit0_error` is the error
#' message in refitting the model
#' (`ptable0`), if
#' any. If
#' no error, it is `NA`.
#'
#' - `vcov_ok` is `TRUE` if the
#' variance-covariance matrix of the
#' estimates
#' can be computed without error nor
#' warning. `FALSE` otherwise.
#'
#' - `vcov_msg` is the message generated
#' when using [lavaan::lavInspect()] to
#' get the variance-covariance matrix
#' of the parameter estimates of the
#' refitted model. If `TRUE`, then no
#' error nor warning. Can be used for
#' diagnostic purposes.
#'
#' - `converged`: Whether refitting
#' the modified model (`ptable0`)
#' converged.
#'
#' - `post_check_passed`: Whether the
#' solution of the modified model
#' (`ptable0`) passed
#' `lavaan`'s post check.
#'
#' - `post_check_msg`: If the solution
#' failed `lavaan`'s post check,
#' it stores the warning message.
#' If the solution passes the check,
#' it is `NA`.
#'
#' - `fit_not_ok`: If the fit failed
#' for some reasons, the fit object,
#' if available, is stored in this
#' element rather than in `fit0`. such
#' that the fit object can be retrieved
#' for diagnostic purposed if necessary.
#'
#' - `df_diff_one`: Whether the
#' difference in model degrees of
#' freedom between the modified model
#' and the original model is one. If
#' a variance is fitted to zero, related
#' covariance(s) is/are also fitted to
#' zero and so the difference in
#' model degrees of freedom can be
#' legitimately greater than one.
#'
#' - `se_force_standard`: Whether
#' `se` was forced to be `"standard"`
#' even if it is `"bootstrap"` in
#' `fit`. If `FALSE`, then either
#' `se` is not `"bootstrap"` in
#' `fit` or it was not changed in
#' fitting the restricted model.
#'
#' @param fit A `lavaan`-class object.
#'
#' @param par_id An integer. The row
#' number of the free parameter in the
#' parameter table of `fit` to be
#' fixed.
#'
#' @param store_fit Logical. If `TRUE`,
#' `fit` will be stored in the output.
#' Default is `FALSE`.
#'
#' @param se_keep_bootstrap Logical.
#' If `TRUE` and `fit` used
#' bootstrapping standard error
#' (with `se = "bootstrap"`), bootstrapping
#' will also be use in fitting the
#' restricted model. If `FALSE`, the
#' default, then `se` will be set
#' to `"standard"` if it is `"bootstrap"`
#' in `fit`, to speed up the computation.
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
#' out <- fix_to_zero(fit, par_id = 15)
#' summary(out$fit0)
#'
#' @seealso [lrtp()]
#'
#' @export

fix_to_zero <- function(fit,
                        par_id,
                        store_fit = FALSE,
                        se_keep_bootstrap = FALSE) {
    if (isFALSE(inherits(fit, "lavaan"))) {
        stop("The fit object is not a lavaan object.")
      }
    ptable <- lavaan::parameterTable(fit)
    # If it is a user-parameter, do not check "free"
    if (ptable[par_id, "op"] != ":=") {
        if (isFALSE(ptable[par_id, "free"] > 0)) {
            stop("The target parameter is not free in the object.")
          }
      }
    par_lhs <- ptable[par_id, "lhs"]
    par_op <- ptable[par_id, "op"]
    par_rhs <- ptable[par_id, "rhs"]
    par_gp <- ptable[par_id, "group"]
    ptable_i <- ptable
    if (constrained_by_plabel(ptable = ptable, par_id = par_id)) {
        ptable_i <- lavaan::lav_partable_add(ptable,
                                             list(lhs = ptable[par_id, "plabel"],
                                                  op = "==",
                                                  rhs = "0"))
      } else if (ptable_i[par_id, "label"] != "") {
        ptable_i <- lavaan::lav_partable_add(ptable,
                                             list(lhs = ptable[par_id, "label"],
                                                  op = "==",
                                                  rhs = "0"))
      } else {
        ptable_i[par_id, "free"] <- 0
        ptable_i[par_id, "ustart"] <- 0
        ptable_i[par_id, "start"] <- 0
        ptable_i[par_id, "est"] <- 0
      }
    # Is it a variance?
    if ((par_lhs == par_rhs) && (par_op == "~~")) {
        is_variance <- TRUE
        # Fix covariance to zero
        tmp1 <- (ptable_i$lhs == par_lhs) &
                (ptable_i$op == "~~") &
                (ptable_i$rhs != par_lhs) &
                (ptable_i$group == par_gp)
        tmp2 <- (ptable_i$rhs == par_lhs) &
                (ptable_i$op == "~~") &
                (ptable_i$lhs != par_lhs) &
                (ptable_i$group == par_gp)
        tmp <- tmp1 | tmp2
        # TODO:
        # - May need to do the same check about labels and plabels
        ptable_i$free[tmp] <- 0
        ptable_i$ustart[tmp] <- 0
        ptable_i$start[tmp] <- 0
        ptable_i$est[tmp] <- 0
      } else {
        is_variance <- FALSE
      }
    slot_opt <- fit@Options
    # slot_pat <- fit@ParTable
    # slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    se_force_standard <- FALSE
    if (("bootstrap" %in% slot_opt$se) && !se_keep_bootstrap) {
        se_force_standard <- TRUE
        slot_opt$se <- "standard"
      }

    fit0_error <- tryCatch(suppressWarnings(fit_i <- lavaan::lavaan(
                                model = ptable_i,
                                slotOptions = slot_opt,
                                slotSampleStats = slot_smp,
                                slotData = slot_dat)),
                            error = function(e) e)
    # Retry with automatic starting values and random start
    if (!lavaan::lavTech(fit_i, "converged") &&
      lavaan::lavTech(fit_i, "iterations") == 0) {
        ptable_i_no_start <-
          ptable_i[, !colnames(ptable_i) %in% c("start", "est", "se")]
        fit0_error <- tryCatch(suppressWarnings(fit_i <- lavaan::lavaan(
                                     model = ptable_i_no_start,
                                     slotOptions = slot_opt,
                                     slotSampleStats = slot_smp,
                                     slotData = slot_dat)),
                                 error = function(e) e)
    }
    fit0_has_error <- inherits(fit0_error, "error")
    if (fit0_has_error) {
        fit_i <- NA
        vcov_msg <- NA
      } else {

        fit0_converged <- lavaan::lavTech(fit_i, "converged")

        # fit0_check_output stores the warning, if any.
        # TRUE is the check is passed
        fit0_check_output <- tryCatch(lavaan::lavTech(fit_i, "post.check"),
                                      warning = function(w) w)

        df <- lavaan::fitMeasures(fit, "df")
        if (!fit0_converged) {
            slot_opt2 <- slot_opt
            slot_opt2$optim.force.converged <- TRUE
            slot_opt2$do.fit <- FALSE
            tmp_error <- tryCatch(suppressWarnings(fit_tmp <- lavaan::lavaan(
                                        model = ptable_i,
                                        slotOptions = slot_opt2,
                                        slotSampleStats = slot_smp,
                                        slotData = slot_dat)),
                                    error = function(e) e)
            if (inherits(tmp_error, "error")) {
                fit0_df_diff_one <- NA
              } else {
                df_i <- lavaan::fitMeasures(fit_tmp, "df")
                if (isTRUE((df_i - df) == 1)) {
                    fit0_df_diff_one <- TRUE
                  } else {
                    fit0_df_diff_one <- FALSE
                  }
              }
          } else {
            df_i <- lavaan::fitMeasures(fit_i, "df")
            if (isTRUE((df_i - df) == 1)) {
                fit0_df_diff_one <- TRUE
              } else {
                fit0_df_diff_one <- FALSE
              }
          }

        vcov_msg <- tryCatch(vcov_chk <- lavaan::lavInspect(fit_i, "vcov"),
                            error = function(e) e,
                            warning = function(w) w)
        if (inherits(vcov_msg, "warning") ||
            is.null(vcov_chk) ||
            inherits(vcov_msg, "error")) {
            # stop("VCOV cannot be computed. The model may not be identified")
            fit0_vcov_ok <- FALSE
          } else {
            fit0_vcov_ok <- TRUE
          }

        ptable_out <- lavaan::parameterTable(fit_i)
        if (!isTRUE(all.equal(ptable_out[par_id, "est"], 0))) {
            # stop("Parameter failed to be fixed to zero.")
            fit0_really_zero <- FALSE
          } else {
            fit0_really_zero <- TRUE
          }

        if (!(isTRUE(all.equal(ptable_out[par_id, "se"], 0)) ||
              (is.na(ptable_out[par_id, "se"])))) {
            # stop("Fixed parameter does not have 0 SE.")
            fit0_really_zero <- FALSE
          } else {
            fit0_really_zero <- TRUE && fit0_really_zero
          }
      }
    # Accept the Heywood case result
    # NOTE: fit0_check_passed may not be a logical
    if (!all(fit0_converged,
             fit0_vcov_ok,
             ifelse(is_variance, TRUE, fit0_df_diff_one))) {
        fit_not_ok <- fit_i
        fit_i <- NA
      } else {
        fit_not_ok <- NA
      }
    out <- list(fit0 = fit_i,
                fit1 = NULL,
                par_id = par_id,
                call = match.call(),
                ptable0 = ptable_i,
                fit0_error = ifelse(fit0_has_error,
                                    fit0_error,
                                    NA),
                vcov_ok = fit0_vcov_ok,
                vcov_msg = vcov_msg,
                converged = fit0_converged,
                post_check_passed = isTRUE(fit0_check_output),
                post_check_msg = ifelse(inherits(fit0_check_output, "warning"),
                                        fit0_check_output,
                                        NA),
                fit_not_ok = fit_not_ok,
                df_diff_one = fit0_df_diff_one,
                se_force_standard = se_force_standard)
    if (store_fit) {
        out$fit1 <- fit
      }
    class(out) <- c("fix_to_zero", class(out))
    out
  }

#' @noRd

constrained_by_plabel <- function(ptable,
                                  par_id) {
    plabel_i <- ptable[par_id, "plabel"]
    ids_eq <- which(ptable$op == "==")
    if (length(ids_eq) == 0) {
        return(FALSE)
      }
    plabels_eq <- unique(c(ptable$lhs[ids_eq],
                           ptable$rhs[ids_eq]))
    if (plabel_i %in% plabels_eq) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }
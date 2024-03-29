#' @title Fix a Free Parameter To Zero And Fit a Model Again
#'
#' @description It fixes a designated free parameter
#' in a lavaan object to zero and refit the model.
#'
#' @details It modifies the parameter
#' table of a `lavaan`-class object and
#' then fits the model again.
#'
#' @return
#' A `fix_to_zero`-class object, which
#' is a list with these elements:call
#'
#' - `fit0` is the `lavaan` output of the
#' refitted object. `NA` if the fit
#' failed for some reasons. A model
#' must converged, passed `lavaan`'s
#' post check, variance covariance
#' matrix of estimates can be computed,
#' and has one more degree of freedom
#' than the original model to be
#' considered acceptable.
#'
#' - `fit1` is the original `lavaan`
#' output if `store_fit` is `TRUE`. It
#' is `NULL` otherwise
#'
#' - `par_id` is the row number of the
#' designated free parameter.
#'
#' - `call` is the original call to this
#' function.
#'
#' - `ptable0` is the parameter table
#' of with the designated parameter fixed
#' to zero. It can be used for diagnostic
#' purpose if the fit failed.
#'
#' - `fit0_error` is the error
#' message in refitting, if any. If
#' no error, it is `NA``.
#'
#' - `vcov_ok` is `TRUE` if the variance
#' covariance matrix of the estimates
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
#' - `converged`: Whether the modified
#' model converged.
#'
#' - `post_check_passed`: Whether the
#' solution of the modified model passed
#' `lavaan`s post check.
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
#' and the original model is one.
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
#' @export

fix_to_zero <- function(fit,
                        par_id,
                        store_fit = FALSE) {
    if (isFALSE(inherits(fit, "lavaan"))) {
        stop("The fit object is not a lavaan object.")
      }
    ptable <- lavaan::parameterTable(fit)
    if (isFALSE(ptable[par_id, "free"] > 0)) {
        stop("The target parameter is not free in the object.")
      }
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

    slot_opt <- fit@Options
    slot_pat <- fit@ParTable
    slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    fit0_error <- tryCatch(suppressWarnings(fit_i <- lavaan::lavaan(
                                model = ptable_i,
                                slotOptions = slot_opt,
                                slotSampleStats = slot_smp,
                                slotData = slot_dat)),
                            error = function(e) e)
    fit0_has_error <- inherits(fit0_error, "error")
    if (fit0_has_error) {
        fit_i <- NA
        vcov_msg <- NA
      } else {

        fit0_converged <- lavaan::lavTech(fit_i, "converged")

        fit0_check_passed <- suppressWarnings(lavaan::lavTech(fit_i, "post.check"))

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

    if (!all(fit0_converged,
             fit0_check_passed,
             fit0_vcov_ok,
             fit0_df_diff_one)) {
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
                post_check_passed = fit0_check_passed,
                fit_not_ok = fit_not_ok,
                df_diff_one = fit0_df_diff_one)
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
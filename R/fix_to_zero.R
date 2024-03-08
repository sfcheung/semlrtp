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
#' is a list with the element `fit0`
#' storing the `lavaan` output of the
#' refitted object, `fit1` storing the
#' original `lavaan` output if requested
#' (`NULL` otherwise), `par_id` storing
#' the row number of the designated free
#' parameter, and `call` storing the
#' original call.
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
    ptable_i[par_id, "free"] <- 0
    ptable_i[par_id, "ustart"] <- 0
    ptable_i[par_id, "start"] <- 0
    ptable_i[par_id, "est"] <- 0

    slot_opt <- fit@Options
    slot_pat <- fit@ParTable
    slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    suppressWarnings(fit_i <- lavaan::lavaan(
                            model = ptable_i,
                            slotOptions = slot_opt,
                            slotSampleStats = slot_smp,
                            slotData = slot_dat))

    # Check df
    df <- lavaan::fitMeasures(fit, "df")
    df_i <- lavaan::fitMeasures(fit_i, "df")
    if (isTRUE(df_i - df != 1)) {
        stop("Failed to make a one-df change.")
      }
    ptable_out <- lavaan::parameterTable(fit_i)
    out <- list(fit0 = fit_i,
                fit1 = NULL,
                par_id = par_id,
                call = match.call())
    if (store_fit) {
        out$fit1 <- fit
      }
    class(out) <- c("fix_to_zero", class(out))
    out
  }
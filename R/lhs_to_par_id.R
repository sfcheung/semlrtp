#' @title Convert Model Syntax To Parameter ID
#'
#' @description Convert a string of the
#' form "lhs-op-rhs" or "lhs-op-rhs.gX"
#' to the row number of a parameter
#' in the parameter table.
#'
#' @details This internal function is
#' to be used by function like [lrt()].
#'
#' @return
#' An integer. The row number of the
#' free parameter in the parameter table.
#'
#' @param fit A `lavaan`-class object.
#'
#' @param par A string, which must be
#' a valid `lavaan` model syntax.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lrt()]
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd

lhs_to_par_id <- function(fit = NULL,
                          par = NULL,
                          group = NULL) {
    if (!isTRUE(is.character(par))) {
        stop("par is not a string.")
      }
    if (length(par) != 1) {
        stop("par must be one single string.")
      }
    pt <- lavaan::parameterTable(fit)
    ngroups <- lavaan::lavInspect(fit, "ngroups")
    has_groups <- isTRUE(ngroups > 1)
    if (has_groups && is.null(group)) {
        stop("group must be set for a multigroup model.")
      }
    if (has_groups) {
        gplabels <- lavaan::lavInspect(fit, "group.label")
        if (is.character(group)) {
            gpid <- match(group, gplabels)
            if (is.na(gpid)) {
                stop("No group labelled ",
                     sQuote(group),
                     ".")
              }
          } else {
            if (!is.numeric(group)) {
                stop("group must be either a string or a number.")
              }
            if (!isTRUE(group %in% seq_len(ngroups))) {
                stop("group is not one of the group IDs.")
              }
            gpid <- group
          }
      } else {
        gplables <- NULL
        gpid <- NULL
      }

    par_syn <- lavaan::lavParseModelString(par,
                                           parser = "new",
                                           as.data.frame. = TRUE)
    if (has_groups) {
        par_syn$group <- gpid
      } else {
        par_syn$group <- 1
      }
    par_syn <- par_syn[, c("lhs", "op", "rhs", "group")]
    # For `~~`
    if (identical(par_syn$op, "~~")) {
        tmp <- par_syn
        tmp$lhs <- par_syn$rhs
        tmp$rhs <- par_syn$lhs
        par_syn <- rbind(par_syn,
                         tmp)
      }
    pt2 <- merge(par_syn,
                 pt[, c("lhs", "op", "rhs", "group", "id")])
    if (nrow(pt2) == 0) {
        stop(par,
             " not found in the parameter table.")
      }
    return(pt2$id)
  }
#' @title Summary by functional group
#'
#' @description
#' Summary of ground layer observations, disaggregated by functional
#'      groups.
#'
#' @param x \code{"grlyr"} object from \code{calc_biomass()}.
#'
#' @param eachplot default \code{FALSE} returns values as the mean
#'      across all plots in \code{x}; \code{TRUE} returns values as
#'      the mean of each functional group within each plot (and may
#'      exclude functional groups unobserved on a given plot).
#'
#' @param ... further arguments (currently ignored).
#'
#' @return
#' A data.frame with estimates for each functional group 'fg',
#'      including 'mass', 'masssd', 'c', 'csd', 'n', 'nsd' (all in kg
#'      per ha), as well as 'vol' and 'volsd' (cubic meters per ha),
#'      and 'cover' and 'coversd' (percentage points per plot).
#'
#' @details
#' Gives basic descriptive summaries, disaggregated by functional
#'      groups.
#'
#' @examples
#' data(est)
#' x <- calc_biomass(est)
#' summary_fg(x)
#' summary_fg(x, eachplot=TRUE)
#'
#' @rdname summary_plot
# ### unexported SE, returns '0' instead of NA for zero-length vectors
#`se` <- function(a, na.rm=TRUE) {
#        if (length(a) < 2) {
#                return(0L)
#        } else {
#                sd(a, na.rm=na.rm) / sqrt(length(na.omit(a))-1)
#        }
#}
##' @export
##' @rdname summary_plot
#`summary_plot` <- function(...) { NULL }
NULL

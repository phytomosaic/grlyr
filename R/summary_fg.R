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
#' @export
#' @rdname summary_fg
### summarize
`summary_fg` <- function(x, eachplot=FALSE, ...){
        if (!inherits(x, "grlyr")) {
                stop('must inherit from `grlyr`')
        }
        # mean and SE of functional groups within each plot...
        fg_inplot <- plyr::ddply(
                x, plyr::.(fg, plot), plyr::summarize,
                massp  = mean(mass*100, na.rm=T),   # kg/ha
                masssd = grlyr:::se(mass*100, na.rm=T),     # kg/ha
                c      = mean(predC*100, na.rm=T),  # kg/ha
                csd    = grlyr:::se(predC*100, na.rm=T),    # kg/ha
                n      = mean(predN*100, na.rm=T),  # kg/ha
                nsd    = grlyr:::se(predN*100, na.rm=T),    # kg/ha
                vol    = mean(volume*0.1, na.rm=T), # m3/ha
                volsd  = grlyr:::se(volume*0.1, na.rm=T),   # m3/ha
                coverp = mean(cover*100,na.rm=T),   # pct points
                coversd= grlyr:::se(cover*100, na.rm=T),    # pct points
                .progress='time' )
        names(fg_inplot) <- gsub('\\p$', '', names(fg_inplot))
        if (eachplot){
                return(fg_inplot)
        }
        # ...then across all plots in the dataset
        fg_allplot <- plyr::ddply(
                fg_inplot, plyr::.(fg), plyr::summarize,
                mass   = mean(mass,   na.rm=T), # kg/ha
                masssd = mean(masssd, na.rm=T), # kg/ha
                c      = mean(c,      na.rm=T), # kg/ha
                csd    = mean(csd,    na.rm=T), # kg/ha
                n      = mean(n,      na.rm=T), # kg/ha
                nsd    = mean(nsd,    na.rm=T), # kg/ha
                vol    = mean(vol,    na.rm=T), # m3/ha
                volsd  = mean(volsd,  na.rm=T), # m3/ha
                cover  = mean(cover, na.rm=T), # pct points
                coversd= mean(coversd,na.rm=T), # pct points
                .progress='time' )
        fg_allplot <- data.frame(
                fg=fg_allplot[,1], round(fg_allplot[,-1], 1))
        fg_allplot
}
### unexported SE, returns '0' instead of NA for zero-length vectors
`se` <- function(a, na.rm=TRUE) {
        if (length(a) < 2) {
                return(0L)
        } else {
                sd(a, na.rm=na.rm) / sqrt(length(na.omit(a))-1)
        }
}

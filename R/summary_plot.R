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
### summarize for PLOTS:
`summary_plot` <- function(x, ...){
     if (!inherits(x, "grlyr")) {
          stop('must inherit from `grlyr`')
     }
     cat('step 1 of 3\n')
     # first, sum per mq for ea 'type', then both moss/lich summed
     y <- plyr::ddply(x, plyr::.(mpid, type), plyr::summarize,
                      mq_mass = sum(mass, na.rm=TRUE),
                      .progress='time')
     y <- reshape2::dcast(
          y, mpid ~ type, fill=0, value.var=c('mq_mass'))
     y$total <- y$lich + y$moss
     y <- plyr::join(x, y, by=c('mpid'), type='full', match='all')
     y <- plyr::rename(y, c('moss'='mq_moss', 'lich'='mq_lich',
                            'total'='mq_total'))
     cat('step 2 of 3\n')
     # sum *within* microquads
     y <- plyr::ddply(y, plyr::.(mpid), plyr::mutate,
                      mq_c      = sum(predC, na.rm=TRUE), # g/mq
                      mq_n      = sum(predN, na.rm=TRUE), # g/mq
                      mq_vol    = sum(volume,na.rm=TRUE), # cm3/mq
                      mq_cover  = sum(cover, na.rm=TRUE), # %/mq
                      .progress='time')
     cat('step 3 of 3\n')
     # aggregate to plot-level
     d <- plyr::ddply(y, plyr::.(plot), plyr::summarize,
                      total_mn = mean(mq_total*100,na.rm=T),# kg/ha
                      total_sd =   sd(mq_total*100,na.rm=T),# kg/ha
                      lich_mn  = mean(mq_lich*100,na.rm=T), # kg/ha
                      lich_sd  =   sd(mq_lich*100,na.rm=T), # kg/ha
                      moss_mn  = mean(mq_moss*100,na.rm=T), # kg/ha
                      moss_sd  =   sd(mq_moss*100,na.rm=T), # kg/ha
                      c_mn     = mean(mq_c*100,na.rm=T),    # kg/ha
                      c_sd     =   sd(mq_c*100,na.rm=T),    # kg/ha
                      n_mn     = mean(mq_n*100,na.rm=T),    # kg/ha
                      n_sd     =   sd(mq_n*100,na.rm=T),    # kg/ha
                      vol_mn   = mean(mq_vol*0.1,na.rm=T),  # m3/ha
                      vol_sd   =   sd(mq_vol*0.1,na.rm=T),  # m3/ha
                      cover_mn = mean(mq_cover,na.rm=T),    # %
                      cover_sd =   sd(mq_cover,na.rm=T),    # %
                      depth_mn = mean(depth, na.rm=T),      # cm
                      depth_sd =   sd(depth, na.rm=T),      # cm
                      .progress='time')
     # functional group richness per plot
     d$fgr <- plyr::ddply(y[y$covercm != '0',,drop = T],
                          plyr::.(plot), plyr::summarize,
                          fgr=length(unique(fg)))$fgr
     # order by plot
     d <- plyr::arrange(d, plot)
     d
}

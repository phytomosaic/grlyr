#' @title Summary by plots
#'
#' @description
#' Summary of ground layer observations at the plot-level.
#'
#' @param x \code{"grlyr"} object from \code{calc_biomass()}.
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
#' s <- summary_plot(x)
#'
#' @export
#' @rdname summary_plot
### summarize for PLOTS:
`summary_plot` <- function(x, ...){
     if (!inherits(x, "grlyr")) {
          stop('must inherit from `grlyr`')
     }
     cat('\n...may take a few seconds...\n\n')
     # first, sum per mq for ea 'type', then both moss/lich summed
     y <- plyr::ddply(x, plyr::.(mpid, type), plyr::summarize,
                      mq_mass = sum(mass, na.rm=TRUE))
     y <- reshape2::dcast(
          y, mpid ~ type, fill=0, value.var=c('mq_mass'))
     y$total <- y$lich + y$moss
     y <- plyr::join(x, y, by=c('mpid'), type='full', match='all')
     y <- plyr::rename(y, c('moss'='mq_moss', 'lich'='mq_lich',
                            'total'='mq_total'))
     # sum *within* microquads
     y <- plyr::ddply(y, plyr::.(mpid), plyr::mutate,
                      mq_c      = sum(predC, na.rm=TRUE), # g/mq
                      mq_n      = sum(predN, na.rm=TRUE), # g/mq
                      mq_vol    = sum(volume,na.rm=TRUE), # cm3/mq
                      mq_cover  = sum(cover, na.rm=TRUE)) # %/mq
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
                      depth_sd =   sd(depth, na.rm=T))      # cm
     # functional group richness per plot
     d$fgr <- plyr::ddply(y[y$covercm != '0',,drop = T],
                          plyr::.(plot), plyr::summarize,
                          fgr=length(unique(fg)))$fgr
     # order by plot
     d <- plyr::arrange(d, plot)
     class(d) <- c('summary_plot', class(d))
     d
}

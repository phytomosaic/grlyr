#' @title Facet plot
#'
#' @description
#' Scatterplot or histogram faceted by groups.
#'
#' @param data data.frame in long format.
#'
#' @param x,y name of variable in \code{data} for which values are
#'      plotted.
#'
#' @param by name of grouping variable in \code{data}.
#'
#' @param free one of \code{c('none','x','y','xy')}, defining which
#'      axes will freely vary; otherwise axes will be identical among
#'      all facets.
#'
#' @param type one of \code{'plot'} or \code{'hist'}, defining which
#'      type of plot.
#'
#' @param qline if \code{type = 'hist'}, then \code{qline = TRUE} will
#'      plot vertical lines at the 25/50/75th percentiles.
#'
#' @param ... further arguments passed to plotting functions.
#'
#' @return
#' Plots to device.
#'
#' @details
#' Useful for disaggregating by some grouping variable, either as a
#'      bivariate scatterplot (\code{'type = plot'}) or univariate
#'      histogram (\code{'type = hist'}).
#'
#' @examples
#' data(est)
#' x  <- calc_biomass(est)
#' fg <- summary_fg(x, eachplot=TRUE)
#' fg$logmass <- log10(fg$mass+1)
#' plot_facet(fg, 'cover', 'mass', by='fg', type='plot',
#'            xlab='Plot cover (%)',
#'            ylab=bquote('Biomass (kg' ~ha^-1*')'))
#' plot_facet(fg, 'logmass', by='fg', type='hist', breaks=33,
#'            xlab='Biomass', ylab='Frequency')
#'
#' @export
#' @rdname plot_facet
`plot_facet` <- function(data, x, y, by, free='none', type='plot',
                         qline=TRUE, ...) {
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        if(!free %in% c('none','x','y','xy')) stop('invalid `free`')
        zz <- unique(data[, by]) # groups to split by
        `auto_rowcol` <- function(n = length(zz)) {
                if (n <= 3)
                        c(1, n)
                else if (n <= 6)
                        c(2, (n + 1)%/%2)
                else if (n <= 12)
                        c(3, (n + 2)%/%3)
                else c(ceiling(n/(nr <- ceiling(sqrt(n)))), nr)
        }
        par(mfrow = auto_rowcol(),
            mgp = c(1.5, 0.5, 0),
            mar = c(3.5, 3, 0.5, 0.5),
            oma = c(0, 0, 0, 0),
            bty = 'L',
            las = 1)
        ylim <- xlim <- NULL
        if ((free == 'none' | free == 'x') & type == 'plot') {
                yrng <- range(data[, y], na.rm=TRUE)
                ymin <- yrng[1] - (diff(yrng) * 0.1)
                ymax <- yrng[2] + (diff(yrng) * 0.1)
                ylim <- c(ymin, ymax)
        }
        if (free == 'none' | free == 'y' | type == 'hist') {
                xrng <- range(data[, x], na.rm=TRUE)
                xmin <- xrng[1] - (diff(xrng) * 0.1)
                xmax <- xrng[2] + (diff(xrng) * 0.1)
                xlim <- c(xmin, xmax)
        }
        if (type == 'plot') {
                for (ii in zz) {
                        tmp <- data[data[, by] %in% ii, ]
                        plot(tmp[, x], tmp[, y], xlim=xlim,
                             ylim=ylim, main='', ...)
                        text(x = graphics::grconvertX(
                                1, from = "npc", to = "user"),
                             y = graphics::grconvertY(
                                     0.95, from = "npc", to = "user"),
                             labels = ii, adj = c(1,.5))
                }
        } else if (type == 'hist') {
                for (ii in zz) {
                        tmp <- data[data[, by] %in% ii, ]
                        hist(tmp[, x], xlim = xlim, ylim = ylim,
                             col='#00000050', main='', ...)
                        box(bty='L')
                        text(x = graphics::grconvertX(
                                1, from = "npc", to = "user"),
                             y = graphics::grconvertY(
                                     0.95, from = "npc", to = "user"),
                             labels = ii, adj = c(1,.5))
                        if (qline) {
                                abline(v=quantile(
                                        tmp[, x], c(.25,.5,.75)),
                                       col=c(3,2,3), lty=2, lwd=2)
                        }
                }
        }
}

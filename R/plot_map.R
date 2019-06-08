#' @title Map of estimation plots
#'
#' @description
#' Map geographic plot locations, colored by value.
#'
#' @param x \code{'summary_plot'} object from function of the same
#'      name.
#'
#' @param xvar name of variable in \code{x} for which colors are
#'      rendered.
#'
#' @param log logical, use \code{'log10(x+1'} transformation (default)
#'      or not.
#'
#' @param col a vector of colors.  If missing (default) then colors
#'      follow \code{\link[viridis]{inferno}}.
#'
#' @param xlab,ylab optional axis labels.
#'
#' @param ... further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return
#' Plots to device.
#'
#' @details
#' Color values are relativized for each graphical image; plotting
#'      absolute values (when these differ among several plots) may
#'      require manually feeding a color vector in the proper order.
#'
#' @examples
#' data(est)
#' x <- calc_biomass(est)
#' s <- summary_plot(x)
#' s$lat <- x$lat[match(s$plot, x$plot)]  # match lat/lon to s
#' s$lon <- x$lon[match(s$plot, x$plot)]  # match lat/lon to s
#' par(mfrow=c(2,2))
#' plot_map(s, total_mn, main='Total biomass')
#' plot_map(s, moss_mn, main='Moss biomass')
#' plot_map(s, lich_mn, main='Lichen biomass')
#' plot_map(s, fgr, main='Functional grp richness')
#'
#' @export
#' @rdname plot_map
`plot_map` <- function(x, xvar, log = TRUE, col, xlab, ylab, ...){
     if (!inherits(x, 'summary_plot')) {
          stop('must inherit from `summary_plot`')
     }
     xvar <- paste0(substitute(xvar))
     z <- if (log) log10(x[,xvar]+1) else x[,xvar]
     if (missing(col)){
          col <- colvec(z, 99, alpha=0.95)
     }
     if (missing(ylab)){
          ylab <- 'Latitude (\U00B0)'
     }
     if (missing(xlab)){
          xlab <- 'Longitude (\U00B0)'
     }
     plot(x$lon, x$lat, col=col, xlab=xlab, ylab=ylab,
          pch=16, las=1, bty='L', ...)
}
### unexported color vector function
`colvec` <- function (x, n = 99, alpha = 0.9,
                      begin = 0.2, end = 0.9, dir = 1, pal, ...) {
     if (is.factor(x)) {
          n <- nlevels(x)
     }
     if (missing(pal)) {
          pal <- suppressMessages(
               viridisLite::inferno(
                    n = n,
                    alpha = alpha,
                    begin = begin,
                    end = end, direction = dir))
     }
     pal[cut(as.numeric(x), breaks = length(pal),
             include.lowest = TRUE)]
}

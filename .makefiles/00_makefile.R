######################################################################

### install and load 'grlyr' package
rm(list=ls())
devtools::install_github('phytomosaic/grlyr')
require(grlyr)

### for further information
citation('grlyr') # please cite in publications
?est              # find help file

### load data: FIA interior Alaska estimation points
data(est)

### calculate biomass using the calc_biomass function
x <- calc_biomass(est)
rm(est)

### summary by functional groups
summary_fg(x)
summary_fg(x, eachplot=TRUE)

### summary by plots
s <- summary_plot(x)

### plots and maps
s$lat <- x$lat[match(s$plot, x$plot)]  # match lat/lon to s
s$lon <- x$lon[match(s$plot, x$plot)]  # match lat/lon to s
par(mfrow=c(2,2))
plot_map(s, total_mn, main='Total biomass')
plot_map(s, moss_mn, main='Moss biomass')
plot_map(s, lich_mn, main='Lichen biomass')
plot_map(s, fgr, main='Functional grp richness')

### scatterplot or histogram faceted by groups
fg <- summary_fg(x, eachplot=TRUE)
fg$logmass <- log10(fg$mass+1)
plot_facet(fg, 'cover', 'mass', by='fg', type='plot',
           xlab='Plot cover (%)',
           ylab=bquote('Biomass (kg' ~ha^-1*')'))
plot_facet(fg, 'logmass', by='fg', type='hist', breaks=33,
           xlab='Biomass', ylab='Frequency')







# for disaggregating by functional groups
`plot_map_fg` <- function(x, xvar, log = TRUE, ...){
        xvar <- substitute(xvar)
        tmp  <- ddply(x, .(plot, fg), summarize, kgha=mean(xvar, na.rm=T))
        tmp$lat  <- x$lat [match(tmp$plot, x$plot)]         # assign lat
        tmp$lon  <- x$lon [match(tmp$plot, x$plot)]         # assign lon
        ufg <- sort(unique(tmp$fg))
        nfg <- length(ufg)
        ecole::set_par(nfg)
        par(mfrow=c(4,4), las=1, bty='L')
        for (i in 1:nfg) {
                i <- 1
                o <- tmp[tmp$fg == as.character(ufg[i]),]
                plot_map(o, kgha, cex=0.5)
        }
}
plot_map_fg(d, total_mn, cex=0.5)
ecole::set_par

tmp <- ddply(x, .(plot, fg), summarize, kgha=mean(xvar, na.rm=T))
tmp$lat  <- x$lat [match(tmp$plot, x$plot)]         # assign lat
tmp$lon  <- x$lon [match(tmp$plot, x$plot)]         # assign lon
ufg <- sort(unique(tmp$fg))
nfg <- length(ufg)
ecole::set_par(nfg)
par(mfrow=c(4,4), las=1, bty='L')
for (i in 1:nfg) {
        i <- 1
        o <- tmp[tmp$fg == as.character(ufg[i]),]
        plot_map(o, kgha, cex=0.5)
}





plot(tmp$kgha, d$total_mn)
head(d)

tmp      <- ddply(x, .(plot, fg), summarize, kgha=mean(mass*100, na.rm=T))
tmp$lat  <- x$lat [match(tmp$plot, x$plot)]         # assign lat
tmp$lon  <- x$lon [match(tmp$plot, x$plot)]         # assign lon
plot_map(tmp, hill, cex=0.5)





# # Biomass map (faceted by fxnl grp)
# tmp <- ddply(x, .(plot, fg), summarize, kgha=mean(mass*100, na.rm=T))
# tmp$lat  <- x$lat [match(tmp$plot, x$plot)]         # assign lat
# tmp$lon  <- x$lon [match(tmp$plot, x$plot)]         # assign lon
# p10 <- ggplot(tmp, aes(x=lon, y=lat, group=fg, colour=log(kgha,10))) +
#      labs(y='Latitude', x='Longitude') +
#      coord_map('mercator',xlim=c(-108.45,-108.36),ylim=c(46.51,46.6)) +
#      geom_point(shape=19, alpha=.5, size=rel(1.25)) +
#      facet_wrap(~fg, nrow=3) +
#      theme(strip.background=element_rect(fill='grey90'),
#            legend.position=c(.7,.1),
#            legend.direction=('horizontal'))+
#      scale_colour_gradient(name='Log(mass),\n   kg/ha',
#                             low='grey90', high='grey10') +
#      theme(axis.text.x =element_text(size=9, hjust=1, angle=45),
#            axis.text.y =element_text(size=9, angle=0)) +
#      theme_classic()
# # tiff('map_biomass_byfxnlgrp.tif',width=9,height=6,units='in',res=750)
# print(p10)
# # dev.off()
# rm(tmp)
# # map Combined biomass
# p1 <- ggplot(d, aes(x=lon, y=lat, colour=total)) +
#      labs(y='Latitude', x='Longitude') +
#      coord_map('mercator',xlim=c(-108.45,-108.36),ylim=c(46.51,46.6)) +
#      geom_point(shape=19, alpha=.5, size=rel(1.25)) +
#      theme(strip.background=element_rect(fill='grey90'),
#            legend.position=c(.7,.1),
#            legend.direction=('horizontal'))+
#      scale_colour_gradient(name='Mass (kg/ha)',
#                            low='grey90', high='grey10') +
#      theme(axis.text.x =element_text(size=9, hjust=1, angle=45),
#            axis.text.y =element_text(size=9, angle=0)) +
#      theme_classic()
# # map FGR
# p2 <- ggplot(d, aes(x=lon, y=lat, colour=fgr)) +
#      labs(y='Latitude', x='Longitude') +
#      coord_map('mercator',xlim=c(-108.45,-108.36),ylim=c(46.51,46.6)) +
#      geom_point(shape=19, alpha=.5, size=rel(1.25)) +
#      theme(strip.background=element_rect(fill='grey90'),
#            legend.position=c(.7,.1),
#            legend.direction=('horizontal'))+
#      scale_colour_manual(name='FGR',
#                          values=c('grey90','grey50','grey10')) +
#      theme(axis.text.x =element_text(size=9, hjust=1, angle=45),
#            axis.text.y =element_text(size=9, angle=0)) +
#      theme_classic()
# # map Hill number a=1
# p3 <- ggplot(d, aes(x=lon, y=lat, colour=hill)) +
#      labs(y='Latitude', x='Longitude') +
#      coord_map('mercator',xlim=c(-108.45,-108.36),ylim=c(46.51,46.6)) +
#      geom_point(shape=19, alpha=.5, size=rel(1.25)) +
#      theme(strip.background=element_rect(fill='grey90'),
#            legend.position=c(.7,.1),
#            legend.direction=('horizontal'))+
#      scale_colour_gradient(name='Hill',low='grey90', high='grey10') +
#      theme(axis.text.x =element_text(size=9, hjust=1, angle=45),
#            axis.text.y =element_text(size=9, angle=0)) +
#      theme_classic()
# # tiff('map_fxnlrich.tif',width=5.5, height=5.5, units='in',res=750)
# grid.arrange(p1,p2,p3,nrow=1)
# # dev.off()
# #### end plotting section ####


######  making datasets to save ###################################
# ### 2017 Plant and Soil data (really 2014 intak data)
# rm(list=ls())
# load('~/papers_submitted/_old_2011-2017/2017_PlantAndSoil_interiorAKcarbon/revised_31Aug2017/Ch2_S2_data.rda')
# rm(d1,d2,d3,d4,d)
# est <- df
# ### fuzz coordinates before making public data
# est$nlat <- est$lat
# est$nlon <- est$lon
# est$lat  <- est$lon <- NA
# set.seed(271)
# for (i in unique(est$plot)){
#         est$lon[est$plot == i] <- est$nlon[est$plot == i] +
#                 runif(1, -0.1, 0.1)
#         est$lat[est$plot == i] <- est$nlat[est$plot == i] +
#                 runif(1, -0.1, 0.1)
# }
# # est <- est[,!colnames(est) %in% c('nlon','nlat','hist_office_elev')]
# names(est)[names(est)=='gf'] <- 'fg'
# names(est)[names(est)=='pid'] <- 'plot'
# est$depth <- est$depth / 2.54
# est$depth <- as.numeric(
#         as.character(
#                 cut(est$depth,
#                     breaks=c(0,0.3125*(2^(0:7))),
#                     labels=0.3125*(2^(0:7))))
# )
# keeps <- c('plot', 'microquad', 'fg', 'cover', 'depth',
#            'subp', 'transect', 'condid', 'condid_plot',
#            'condid_subp', 'condid_grlyr', 'cond_status_cd',
#            'microquad_status_cd', 'subpcond', 'duffdep', 'litterdep',
#            'treecv', 'shrbcv', 'forbcv', 'grascv', 'seedct',
#            'slope', 'aspect', 'aspfold', 'pdir', 'htld',
#            'fldage', 'gps_elev', 'julian', 'lat', 'lon')
# all(keeps %in% names(est))
# est <- est[,keeps]
# save(est, file='C:/Users/Rob/Documents/_prj/grlyr/data/est.rda')
#
# ### 2013 calibration data
# cal <- read.csv('~/_prj/9_intak/data_2013/data/2013biomass.csv',
#                 header=T)
# names(cal)[names(cal)=='gf'] <- 'fg'
# names(cal)[names(cal)=='biomassid'] <- 'sampleid'
# cal$date <- as.character(cal$date)
# cal$date[cal$date == 2012] <- '8/1/2012'
# cal$date <- as.Date(cal$date, format = '%m/%d/%Y')
# cal$vol  <- cal$depth * cal$area
# cal$dens <- cal$ovendrymass / cal$vol
# cal$fg   <- as.character(cal$fg) # assign <1cm acrocarps to CC
# cal$fg[ cal$depth <= 1 & cal$fg=='A' ] <- 'C'
# cal$fg  <- as.factor(cal$fg)
# cal     <- cal[!colnames(cal)=='oldmass']
# # save(cal, file='C:/Users/Rob/Documents/_prj/grlyr/data/cal.rda')
#
# ### 2016 Milton data
# x <- read.csv('~/_prj/9_intak/2016_MiltonRanch/milton_data.csv',
#               header=T, row.names=1, stringsAsFactors=T)
# head(x)
# dim(x)
# names(x)[names(x)=='gf'] <- 'fg'
#


### all current references
# Calabria, L. M., K. Petersen, S. T. Hamman, and R. J. Smith. 2016.
# Prescribed fire decreases lichen and bryophyte biomass and alters
# functional group composition in Pacific Northwest prairies.
# Northwest Science 90:470–483.
#
# Pattison, R., H.-E. Andersen, A. Gray, B. Schulz, R. J. Smith, and
# S. Jovan. 2018. Forests of the Tanana Valley State Forest and Tetlin
# National Wildlife Refuge, Alaska: results of the 2014 pilot
# inventory. Page 80. Gen. Tech. Rep. PNW-GTR-967, US Department of
# Agriculture, Forest Service, Pacific Northwest Research Station,
# Portland, OR.
#
# Rosso, A., P. Neitlich, and R. J. Smith. 2014. Non-destructive
# lichen biomass estimation in Northwestern Alaska: a comparison of
# methods. PLoS ONE 9:e103739.
#
# Smith, R. J., J. C. Benavides, S. Jovan, M. Amacher, and B. McCune.
# 2015. A rapid method for landscape assessment of carbon storage and
# ecosystem function in moss and lichen ground layers. The Bryologist
# 118:32–45.
#
# Smith, R. J., S. Jovan, A. N. Gray, and B. McCune. 2017. Sensitivity
# of carbon stores in boreal forest moss mats - effects of vegetation,
# topography and climate. Plant and Soil 421:31–42.
#
# Smith, R. J., S. Jovan, and B. McCune. 2014. Ubiquitous moss and
# lichen mats promote forest health. Pages 68–69 in E. Graham and T.
# Huette, editors. Forest Health Conditions in Alaska 2013.
# R10-PR-035. USDA Forest Service, Alaska Region, Anchorage, Alaska.
#
###   END   #########################################################

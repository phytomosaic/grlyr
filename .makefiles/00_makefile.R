######################################################################

### install and load 'grlyr' package
rm(list=ls())
devtools::install_github('phytomosaic/grlyr')
require(grlyr)

### for further information
citation('grlyr') # please cite in publications
?est              # find help file
?launch_app

### load data: FIA interior Alaska estimation points
data(est)
# est <- read.csv('est.csv', header=TRUE) # alternatively, read a csv

### check that column names are correct before proceeding
check_dat(est)  # expect TRUE
# first 5 column names must be "plot","microquad","gf","cover","depth"


e <- est[,1:7]
e <- e[,-6]
f <- function(xx) {
        formatC(xx, width = 2, format = "d", flag = "0")
}
e$mqid <- paste(f(e$plot),  f(e$transect), f(e$microquad), sep = "_")

e <- merge(e,
           data.frame(
                   something = round(rnorm(length(unique(e$mqid)),20,5),1),
                   mqid = unique(e$mqid)),
           by = "mqid")
e <- e[,-1]
check_dat(e)
head(e)


head(e, 20)
cat('...\n')
tail(e, 20)






### calculate biomass using the calc_biomass function
x <- calc_biomass(est)
rm(est)

### summary by functional groups
summary_fg(x)
summary_fg(x, eachplot=TRUE)

### summary by plots
s <- summary_plot(x)

### plots and maps
names(x) %in% c('lat', 'lon')
if (!all(c('lat', 'lon') %in% names(x))) {
        stop('your data need columns `lat` and `lon` to map')
} else {
        s$lat <- x$lat[match(s$plot, x$plot)]  # match lat/lon to s
        s$lon <- x$lon[match(s$plot, x$plot)]  # match lat/lon to s
        par(mfrow=c(2,2))
        plot_map(s, total_mn, main='Total biomass')
        plot_map(s, moss_mn, main='Moss biomass')
        plot_map(s, lich_mn, main='Lichen biomass')
        plot_map(s, fgr, main='Functional grp richness')
}

### scatterplot or histogram faceted by groups
sfg <- summary_fg(x, eachplot=TRUE)
sfg$logmass <- log10(sfg$mass+1)
plot_facet(sfg, 'cover', 'mass', by='fg', type='plot',
           xlab='Plot cover (%)',
           ylab=bquote('Biomass (kg' ~ha^-1*')'))
plot_facet(sfg, 'logmass', by='fg', type='hist', breaks=33,
           xlab='Biomass', ylab='Frequency')
head(sfg)

### can also map occurrences of each fg
sfg     <- summary_fg(x, eachplot=TRUE)
sfg$lat <- x$lat[match(sfg$plot, x$plot)]  # match lat/lon
sfg$lon <- x$lon[match(sfg$plot, x$plot)]  # match lat/lon
plot_facet(sfg, 'lon', 'lat', by='fg', type='plot',
           xlab='Longitude', ylab='Latitude',
           pch=16, col=1, cex=0.8)


###
launch_app()


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
###   END   #########################################################




######################################################################
# Interior Alaska moss and lichen mats
#  Rob Smith, smithr2@oregonstate.edu, Oregon State Univ, 31 Aug 2017
##  CC-BY-SA 4.0 License (Creative Commons Attribution-ShareAlike 4.0)

### for Plant and Soil article, revised 31 Aug 2017

### preamble
rm(list=ls())
pkg <- c('plyr','ggplot2','gridExtra','grid','scales','viridis')
has <- pkg %in% rownames(installed.packages())
if(any(!has))install.packages(pkg[!has])
lapply(pkg, require, character.only = TRUE)
lapply(pkg, citation)  # credit is due these authors
rm(pkg, has)

###
# setwd('C:/Users/Rob/Documents/_prj/1 intak/data_2014/for_r/')
setwd('~/_prj/1 intak/z_ms_PlantAndSoil/revised_31Aug2017/')
###

load('./Ch2_S2_data.rda')
# #    df = each row is an observation for each fxnl grp within a
# #      microplot nested within transects > subplots > plots.
# #    d = each row is an observation for each plot
# #      (aggregated from df), to be split for NPMR regressions;
# #       lat/lon coordinates are fuzzed from FIA.
# #    d1,d2,d3,d4 = predicted values from NPMR regressions.

### most heavy lifting done in HyperNiche v. 2.0, please cite:
# McCune, B. 2006. Nonparametric habitat models with automatic
#    interactions. Journal of Vegetation Science 17:819–830.
# McCune, B., and M. J. Mefford. 2011. HyperNiche. Multiplicative
#    Habitat Modeling. Version 2. MjM Software Design, Gleneden Beach,
#    Oregon.

eb <- element_blank()
theme_set(theme_classic() + theme(
        legend.position=c(.85,.85),
        legend.background = element_rect(color='white'),
        legend.title = element_text(size=10),
        axis.text=element_text(colour='black'),
        plot.background=eb,
        panel.background=eb,
        plot.margin=unit(c(2,1,2,0), 'mm')))

### Fig 01 is the conceptual diagram


### Fig 02  Alaska locator map
p02 <- ggplot(d, aes(x=lon,y=lat)) +
        borders('world','USA:Alaska',colour='black',fill=rgb(0,0,0,.3))+
        borders('world','Canada',colour='black',fill=rgb(0,0,0,.1))+
        ylab('Latitude') + xlab('Longitude') + coord_map('albers',0,0)+
        coord_map('albers', 50, 70, xlim=c(-160, -130), ylim=c(54,70))+
        geom_point(colour='red',size=rel(1),shape=3)+
        annotate('text',x=-155, y=68, label='Alaska', size=rel(7))+
        theme(plot.margin=unit(c(1,9,1,1), 'mm'),
              axis.title=element_text(size=rel(1.8)),
              axis.text=element_text(size=rel(1.5)),
              axis.ticks=eb,
              panel.grid.major=element_line(color='black',linetype=3))
# tiff('fig_02_locatormap.tif',wid=6.5,hei=6.5,units='in',
#      compr='lzw+p',res=400)
print(p02)
# dev.off()


### Fig 03  Biomass histogram across ALL plots
# tiff('fig_000_biomass_histogram.tif',wid=5,hei=4,
#      units='in',res=400)
ss  <- summary(d$combo)
p03 <- ggplot(d, aes(combo)) +
        geom_histogram(colour='grey50', fill='grey80') +
        labs(x=bquote('Biomass (kg' ~ha^-1*')'), y='Count of plots') +
        scale_y_continuous(breaks=c(1:9), expand=c(0,0)) +
        geom_vline(aes(xintercept=ss[[2]]),  colour='grey20',
                   linetype='dashed', size=1) +
        geom_vline(aes(xintercept=ss[[3]]), colour='red',
                   linetype='dashed', size=1) +
        geom_vline(aes(xintercept=ss[[5]]), colour='grey20',
                   linetype='dashed', size=1) +
        scale_x_continuous(
                trans=log10_trans(),
                breaks=trans_breaks('log10',function(x)10^x),
                labels=trans_format('log10',math_format(10^.x)))
# tiff('fig_03_biomass_hist.tif', wid=5.5,hei=5,units='in',
#      compr='lzw+p',res=400)
print(p03)
# dev.off()


### Fig 04 Biomass histograms, faceted by functional group
#         to replace similar map, just below
tmp <- ddply(df, .(pid, gf), summarize, kgha=mean(mass*100, na.rm=T))
tmp$lat  <- df$lat [match(tmp$pid, df$pid)]         # assign lat
tmp$lon  <- df$lon [match(tmp$pid, df$pid)]         # assign lon
levels(tmp$gf) <- c('Biotic soil crust','Eutrophic lichens',
                    'Forage lichens','Foliose lichens',
                    'Fruticose lichens','N-fixing fol lichens',
                    'N-fixing fru lichens','Feather moss',
                    'N-fixing moss','Sphagnum peatmoss','Turf mosses',
                    'Flat liverworts','Leafy liverworts')
# meds <- aggregate(tmp$kgha, by=list(gf=tmp$gf), median)
p04 <- ggplot(tmp, aes(x=kgha, group=gf)) +
        labs(x=bquote('Biomass (kg' ~ha^-1*')'), y='Count of plots') +
        geom_histogram(colour='grey50', fill='grey80',
                       bins = 16) +
        facet_wrap(~gf, nrow=3) +
        scale_x_continuous(
                trans=log10_trans(),
                breaks=trans_breaks('log10',function(x)10^x),
                labels=trans_format('log10',math_format(10^.x)))+
        # geom_vline(data=meds, aes(xintercept=x, group=gf), linetype='dashed')+
        theme(panel.spacing.y=unit(5,'mm'),
              strip.background=element_rect(colour=NA, fill=NA),
              strip.text=element_text(colour='black',size=8))
# tiff('fig_04_biomass_fxnlgrp.tif',wid=6.5,hei=4.5,units='in',
#      compr='lzw+p',res=400)
print(p04)
# dev.off()


### Fig 06 plot NPMR contours
d2$forb <- d2$forb*100
d4$forb <- d4$forb*100
names(d1) <-names(d2) <-names(d3) <-names(d4) <- c('Step','x','y','z')
p1 <- ggplot(d1, aes(x,y,z=z)) +
        guides(fill=guide_colorbar(title=bquote('C (kg'~ha^-1*')'),
                                   barwidth=0.5, barheight=3)) +
        ylab(bquote('PDIR (MJ '~cm^-2~y^-1*')')) + xlab('Stand age (y)')+
        geom_raster(aes(fill=z)) + stat_contour(colour='black',size=1)+
        scale_fill_viridis(option='B', direction=1, limits=c(0,11100),
                           begin=0, end=.9, discrete=F, na.value=NA)
p2 <- ggplot(d2, aes(x,y,z=z)) +
        guides(fill=guide_colorbar(title=bquote('C (kg'~ha^-1*')'),
                                   barwidth = 0.5, barheight = 3)) +
        ylab(bquote('PDIR (MJ '~cm^-2~y^-1*')')) +xlab('Forb cover (%)')+
        geom_raster(aes(fill=z)) + stat_contour(colour='black',size=1)+
        scale_fill_viridis(option='B', direction=1, limits=c(0,11100),
                           begin=0, end=.9, discrete=F, na.value=NA)
p3 <- ggplot(d3, aes(x,y,z=z)) +
        guides(fill=guide_colorbar(title=bquote('N (kg'~ha^-1*')'),
                                   barwidth = 0.5, barheight = 3)) +
        ylab(bquote('PDIR (MJ '~cm^-2~y^-1*')')) +xlab('Stand age (y)')+
        geom_raster(aes(fill=z)) + stat_contour(colour='black',size=1)+
        scale_fill_viridis(option='B', direction=1, limits=c(0,300),
                           begin=0, end=.9, discrete=F, na.value=NA)
p4 <- ggplot(d4, aes(x,y,z=z)) +
        guides(fill=guide_colorbar(title=bquote('N (kg'~ha^-1*')'),
                                   barwidth = 0.5, barheight = 3)) +
        ylab(bquote('PDIR (MJ '~cm^-2~y^-1*')')) +xlab('Forb cover (%)')+
        geom_raster(aes(fill=z)) + stat_contour(colour='black',size=1)+
        scale_fill_viridis(option='B', direction=1, limits=c(0,300),
                           begin=0, end=.9, discrete=F, na.value=NA)
# tiff('fig_06_contours.tif',wid=6.5,hei=6.5,units='in',
#      compr='lzw+p',res=400)
print(grid.arrange(p1,p2,p3,p4,nrow=2,padding=unit(0,'line')))
# dev.off()


###   begin error propagation analysis   #############################
### pre-process data, using 2013/14 calibration samples
fnm <- '~/_prj/1 intak/data_2014/for_r/intak_joined_17Nov2014.csv'
dfz <- read.csv(fnm, header=T, stringsAsFactors=F)
fnm <- '~/_prj/1 intak/data_2013/data/2013biomass.csv'
df_calib <- read.csv(fnm, header=T, stringsAsFactors=F)
dfz <- rename(dfz, c('public_id_temp' =             'pid',
                     'cover_pct_cd' =               'cvrclass'))
dfz <- dfz[ !(dfz$visit==2), ] # remove 2013 QA plots at duplicate locs
dfz$cover <- dfz$volume <- dfz$preddens <- dfz$mass <- NA
# convert cover classes to midpoint percentage values
dfz$cover <- mapvalues(dfz$cvrclass, from=c(0,1,2,3,4,5,6,7,8,9,10),
                       to=c(0,0.001,0.005,0.015,0.035,0.075,
                            0.175,0.375,0.625,0.85,0.975))
dfz$covercm   <- dfz$cover * 1000    # convert plot cover to square cm
dfz$duffdep   <- dfz$duffdep * 2.54  # convert depth to cm
dfz$litterdep <- dfz$litterdep * 2.54# convert depth to cm
dfz$depth  <- dfz$depth * 2.54       # convert depth to cm
dfz$volume <- dfz$depth * dfz$covercm # calc plot volume in cubic cm
dfz$mpid <- paste(
        formatC(dfz$pid, width=2, format = 'd', flag ='0'),
        formatC(dfz$subp, width=2, format = 'd', flag ='0'),
        formatC(dfz$transect, width=3, format = 'd', flag ='0'),
        formatC(dfz$microquad, width=2, format = 'd', flag ='0'),
        sep='_')
dfz <- arrange(dfz, pid, transect, microquad)

### calc microquad biomass from 2013 *calibration* data
df_calib$vol  <- df_calib$depth * df_calib$area
df_calib$dens <- df_calib$ovendrymass / df_calib$vol
f2 <- deriv3(~Const+a*exp(-b*depth), c('Const','a','b'),
             function(Const,a,b,depth) NULL)
m2 <- nls(dens~f2(Const,a,b,depth),data=df_calib,
          start=list(Const=0.5,a=0.01,b=0.5))
summary(m2)
# predicted density (g/cm^3):
dfz$preddens <- as.vector(predict(m2, newdata=list(depth=dfz$depth)))
dfz$mass     <- dfz$preddens * dfz$volume      # mass (g/microplot)

### aggregate to plot level
`aggro` <- function(x){
        # sum *within* mp (g/microplot)
        mp_mass <- aggregate(x, by=list(mpid=dfz$mpid), sum)
        mp_mass$pid <- sapply(strsplit(mp_mass$mpid, '_'), `[`, 1)
        # average within plot
        plot_mass   <- aggregate(mp_mass$x,by=list(pid=mp_mass$pid),mean)
        plot_mass$x <- plot_mass$x*100
        return(plot_mass$x)
}
obsvd_mass <- aggro(x=dfz$mass) # for each of 96 plots

### bootstrap resampling of calibration samples ! ! ! TIME WARN ! ! !
# root mean square error (RMSE) to assess prediction accuracy
`rmse`   <- function(y, ypred, ...){ sqrt(mean((y-ypred)^2)) }
i        <- 1       # iteration
n        <- 999     # arbitrarily large 999
size     <- 150     # same size as original calibration set
x        <- cbind(dens=df_calib$dens, depth=df_calib$depth)
bootmass <- matrix(nrow=nrow(dfz),ncol=n)# predicted microquad mass
plotmass <- matrix(nrow=96,ncol=n)      # predicted plot mass
calib_err<- rep(NA, n)                  # calibration RMSE
mpid_err <- rep(NA, n)                  # microquad RMSE
plot_err <- rep(NA, n)                  # plot RMSE
while(i < n+1){
        idx   <- sample(x=nrow(x), size=size, replace=T)
        m <- try(nls(dens~f2(Const,a,b,depth),
                     data=data.frame(x[idx,]),
                     start=list(Const=0.5,a=0.01,b=0.5)))
        if(class(m) != 'try-error') {
                calib_err[i]<- sigma(m)
                preddens    <- as.vector(predict(m, list(depth=dfz$depth)))
                bootmass[,i]<- preddens * dfz$volume # mass (g/microplot)
                mpid_err[i] <- rmse(y=dfz$mass, ypred=bootmass[,i])
                plotmass[,i]<- aggro(x=bootmass[,i])
                plot_err[i] <- rmse(y=obsvd_mass, ypred=plotmass[,i])
                i <- i+1
        }
}

### 95% bootstrap CIs for prediction error (RMSE)
quantile(calib_err, c(.025,.975))
quantile(mpid_err, c(.025,.975))
quantile(plot_err, c(.025,.975))
# NB: could also bootstrap microquads to get 95% CIs for plot means...


### Fig 05 uncertainty analysis outcomes
p1 <- ggplot() +
        geom_histogram(aes(x=calib_err), colour='grey50', fill='grey80')+
        labs(x=bquote('Calibration RMSE (g' ~cm^-3*')'),y='Count')+
        scale_y_continuous(expand=c(0,0)) +
        geom_vline(aes(xintercept=quantile(calib_err, c(.025,.975))),
                   colour='black', linetype='dashed', size=1)
p2 <- ggplot() +
        geom_histogram(aes(x=mpid_err), colour='grey50', fill='grey80')+
        labs(x=bquote('Microquad RMSE (g 1000' ~cm^-2*')'),y='Count')+
        scale_y_continuous(expand=c(0,0)) +
        geom_vline(aes(xintercept=quantile(mpid_err, c(.025,.975))),
                   colour='black', linetype='dashed', size=1)
p3 <- ggplot() +
        geom_histogram(aes(x=plot_err), colour='grey50', fill='grey80')+
        labs(x=bquote('Plot RMSE (kg' ~ha^-1*')'),y='Count') +
        scale_y_continuous(expand=c(0,0)) +
        geom_vline(aes(xintercept=quantile(plot_err, c(.025,.975))),
                   colour='black', linetype='dashed', size=1)
### plot em all
# tiff('fig_05_uncertainty.tif',wid=8,hei=2.5,units='in',
#      compr='lzw+p',res=400)
grid.arrange(p1,p2,p3, ncol=3)
# dev.off()

####   end error propagation analysis   ##############################

####   END   #########################################################





###   SANDBOX   ######################################################
###   SANDBOX   ######################################################
###   SANDBOX   ######################################################
###   SANDBOX   ######################################################

# ###  error propagation analysis (analytical)
# `errprop` <- function(r, x, y, z){
#      err <- r*(sqrt( ((0.01/x)^2) + ((10/y)^2) + ((1/z)^2) ))
#      return(err)
# }
# errvec <- errprop(r=df_calib$dens, df_calib$ovendrymass, df_calib$area, df_calib$depth)
# # sd = 0.01431 g/cm3 = avg measurement uncertainty for calibn set
# mean(errvec)
# hist(errvec, breaks=33, col='grey80', las=1) #
# abline(v=median(errvec), col=3, lwd=3)
# abline(v=mean(errvec), col=2, lwd=3)
# sd(df_calib$dens)   # observed sd
# sigma(m2)      # calibration model RMSE, g/cm3
# mean(errvec)   # mean measurement error

# ### calc mass 'ignorant' of fact that density increases with depth
# (ignorant <- mean(df_calib$dens))
# plot(dfz$mass, dfz$preddens, ylim=c(0,0.055), las=1)
# abline(h=ignorant, col=2)
# hist(dfz$preddens - ignorant, breaks=33, col='grey80')
# abline(v=0); abline(v=median(dfz$preddens - ignorant), col=2)
# rm(ignorant)

### alaska DEM map
# require(raster)
# require(rgdal)
# r <- raster('E:/_prj/ms_PlantAndSoil/akshd300m.tif') # the AK DEM
# prj_old <- '+proj=aea +datum=NAD27 +lon0=-154.0 +lat1=55.0 +lat2=65.0'
# prj_pts <- '+proj=longlat +datum=WGS84'
# prj_new <- '+proj=lcc +lat_1=50 +lat_2=70 +lon_0=-150 +datum=WGS84'
# projection(r) <- prj_old
# pts <- data.frame(lon=d$lon*-1, lat=d$lat)
# coordinates(pts) <- c('lon', 'lat')
# projection(pts) <- prj_pts
# npts <- spTransform(pts, prj_new)
# rpts <- spTransform(pts, prj_old)
# fnm <- 'E:/_prj/ms_PlantAndSoil/AK_DEM.tif'
# # projectRaster(from=r, crs=prj_new, filename=fnm) # ! ! ! TIME WARN !
# ###
# r <- raster(fnm) # the AK DEM
# plot(r, las=1)
# str(rpts)
# rpts@data
# # image(r)
# points(0,1500000, pch=16)
# points(535837, 1500000, pch=16, col=2)
# points(pts)
# points(npts)
# points(rpts)
# points(5358371, 11801570)
# col=grey.colors(99)

# ### biomass map (faceted by fxnl grp)
# tmp$lat  <- df$lat [match(tmp$pid, df$pid)]         # assign lat
# tmp$lon  <- df$lon [match(tmp$pid, df$pid)]         # assign lon
# p01 <- ggplot(tmp, aes(x=lon, y=lat, group=gf, colour=log(kgha,10))) +
#      labs(y='Latitude', x='Longitude') +
#      coord_map('mercator', xlim=c(-151, -140.5), ylim=c(62,65.5)) +
#      geom_point(shape=19, alpha=1, size=rel(1)) +
#      facet_wrap(~gf, nrow=3) +
#      theme(strip.background=element_rect(fill=NA),
#            strip.text=element_text(size=7),
#            legend.position=c(.8,.1),
#            legend.direction=('horizontal'),
#            axis.text.x =element_text(size=9, hjust=1, angle=45),
#            axis.text.y =element_text(size=9, angle=0)) +
#      scale_color_viridis(name=bquote('Biomass (kg' ~ha^-1*')'),
#                          labels = c(bquote(10^1),bquote(10^2),
#                                     bquote(10^3),bquote(10^4),
#                                     bquote(10^5)),
#                          option='B',direction=1,begin=0,end=.9) +
#      guides(colour = guide_colorbar(title.position = 'top'))
# # tiff('fig_2_biomassmap.tif',wid=6.5,hei=4.5,units='in',
#         compr='lzw+p',res=400)
# print(p01)
# # dev.off()

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


### launch the web app
launch_app()



######################################################################
### for Plant and Soil article, revised 31 Aug 2017
setwd('~/_prj/1 intak/z_ms_PlantAndSoil/revised_31Aug2017/')
load('./Ch2_S2_data.rda')
###   begin error propagation analysis   #############################
### pre-process data, using 2013/14 calibration samples
fnm <- '~/_prj/1 intak/data_2014/for_r/intak_joined_17Nov2014.csv'
dfz <- read.csv(fnm, header=T, stringsAsFactors=F)
fnm <- '~/_prj/1 intak/data_2013/data/2013biomass.csv'
df_calib <- read.csv(fnm, header=T, stringsAsFactors=F)
dfz <- rename(dfz, c('public_id_temp' =             'pid',
                     'cover_pct_cd' =               'cvrclass'))
dfz <- dfz[ !(dfz$visit==2), ] # remove 2013 QA plots at dupe locs
dfz$cover <- dfz$volume <- dfz$den <- dfz$mass <- NA
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
dfz$den <-as.vector(predict(m2,newdata=list(depth=dfz$depth))) # g/cm3
dfz$mass <- dfz$den * dfz$volume      # mass (g/microplot)

### aggregate to plot level
`aggro` <- function(x){
        # sum *within* mp (g/microplot)
        mp_mass <- aggregate(x, by=list(mpid=dfz$mpid), sum)
        mp_mass$pid <- sapply(strsplit(mp_mass$mpid, '_'), `[`, 1)
        # average within plot
        plot_mass   <- aggregate(mp_mass$x,
                                 by=list(pid=mp_mass$pid),
                                 mean)
        plot_mass$x <- plot_mass$x*100
        return(plot_mass$x)
}
obsvd_mass <- aggro(x=dfz$mass) # for each of 96 plots

### bootstrap resampling of calibration samples ! ! ! TIME WARN ! ! !
#       root mean square error (RMSE) to assess prediction accuracy
`rmse`   <- function(y, ypred, ...){ sqrt(mean((y-ypred)^2)) }
i        <- 1       # iteration
n        <- 999     # arbitrarily large 999
size     <- 150     # same size as original calibration set
x        <- cbind(dens=df_calib$dens, depth=df_calib$depth)
bootmass <- matrix(nrow=nrow(dfz),ncol=n) # predicted microquad mass
plotmass <- matrix(nrow=96,ncol=n)        # predicted plot mass
calib_err<- rep(NA, n)                    # calibration RMSE
mpid_err <- rep(NA, n)                    # microquad RMSE
plot_err <- rep(NA, n)                    # plot RMSE
while(i < n+1){
        idx   <- sample(x=nrow(x), size=size, replace=T)
        m <- try(nls(dens~f2(Const,a,b,depth),
                     data=data.frame(x[idx,]),
                     start=list(Const=0.5,a=0.01,b=0.5)))
        if(class(m) != 'try-error') {
                calib_err[i]<- sigma(m)
                den    <- as.vector(predict(m, list(depth=dfz$depth)))
                bootmass[,i]<- den * dfz$volume # mass (g/microplot)
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
# NB: could also bootstrap MICROQUADS to get 95% CIs for plot means...

### Fig 05 uncertainty analysis outcomes
xl <- bquote('Plot RMSE (kg' ~ha^-1*')')
yl <- 'Count'
hist(calib_err, breaks=33, col='grey80', las=1, xlab=xl, ylab=yl)
abline(v=quantile(calib_err, c(.025,.975)), col=1, lty=2, lwd=2)
box(bty='L')
hist(mpid_err, breaks=33, col='grey80', las=1, xlab=xl, ylab=yl)
abline(v=quantile(mpid_err, c(.025,.975)), col=1, lty=2, lwd=2)
box(bty='L')
hist(plot_err, breaks=33, col='grey80', las=1, xlab=xl, ylab=yl)
abline(v=quantile(plot_err, c(.025,.975)), col=1, lty=2, lwd=2)
box(bty='L')
####   end error propagation analysis   ##############################




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





####   END   #########################################################

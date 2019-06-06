######################################################################

####    Pre-processing data    ####
rm(list=ls())
require(plyr)           # for data processing
require(reshape2)       # for data processing
require(vegan)          # for ecological tasks (Hill numbers and NMS)
require(viridisLite)    # for color scales

# ### to cite in publications, please use:
# citation('grlyr')

### 2013 calibration data
cal <- read.csv('~/_prj/9_intak/data_2013/data/2013biomass.csv',
                header=T)
names(cal)[names(cal)=='gf'] <- 'fg'
names(cal)[names(cal)=='biomassid'] <- 'sampleid'
cal$date <- as.character(cal$date)
cal$date[cal$date == 2012] <- '8/1/2012'
cal$date <- as.Date(cal$date, format = '%m/%d/%Y')
cal$vol  <- cal$depth * cal$area
cal$dens <- cal$ovendrymass / cal$vol
cal$fg   <- as.character(cal$fg) # assign <1cm acrocarps to CC
cal$fg[ cal$depth <= 1 & cal$fg=='A' ] <- 'C'
cal$fg  <- as.factor(cal$fg)
cal     <- cal[!colnames(cal)=='oldmass']
# save(cal, file='C:/Users/Rob/Documents/_prj/grlyr/data/cal.rda')
# load(cal)

# ### 2016 Milton data
# x <- read.csv('~/_prj/9_intak/2016_MiltonRanch/milton_data.csv',
#               header=T, row.names=1, stringsAsFactors=T)
# head(x)
# dim(x)
# names(x)[names(x)=='gf'] <- 'fg'


# ### 2017 Plant and Soil data (really 2014 intak data)
# # ###
# load('~/papers_submitted/_old_2011-2017/2017_PlantAndSoil_interiorAKcarbon/revised_31Aug2017/Ch2_S2_data.rda')
# rm(d1,d2,d3,d4,d)
# est <- df
# ### fuzz coordinates before making public data
# est$nlat <- est$lat
# est$nlon <- est$lon
# est$lat  <- est$lon <- NA
# set.seed(271)
# for (i in unique(est$pid)){
#         est$lon[est$pid == i] <- est$nlon[est$pid == i] +
#                 runif(1, -0.1, 0.1)
#         est$lat[est$pid == i] <- est$nlat[est$pid == i] +
#                 runif(1, -0.1, 0.1)
# }
# est <- est[,!colnames(est) %in% c('nlon','nlat','hist_office_elev')]
# names(est)[names(est)=='gf'] <- 'fg'
# head(est)
# save(est, file='C:/Users/Rob/Documents/_prj/grlyr/data/est.rda')
# # load(est)


x <- est
head(est)

### differing functional grp definitions
fg_forest <- c('CC','CO',
               'LF','LLFOL','LLFRU','LNFOL','LNFRU',
               'MF','MN','MS','MT',
               'VF','VS')
orgtype_f <- c(rep('lich',2),rep('lich',5),rep('moss',4),rep('moss',2))
fg_rangel <- c('CBIND', 'CCYANO', 'CN', 'CO', 'CROCK', 'CSOIL',
               'LF','LLFOL','LLFRU','LNFOL','LNFRU',
               'MF','MN','MS','MT',
               'VF','VS')
orgtype_r <- c(rep('crust',6),rep('lich',5),rep('moss',4),rep('moss',2))

### data checks
# fg <- c('MF', 'MN', 'MS', 'MT', 'MTL', 'VF', 'VS',
#         'LLFOL', 'LLFRU',
#         'CBIND', 'CCYANO', 'CN', 'CO', 'CROCK', 'CSOIL', 'NOS')
# orgtype <- c(rep('moss',7), rep('lich',2), rep('crust',7))
isforest <- all(x$fg %in% fg_forest)
israngel <- all(x$fg %in% fg_rangel)
if (!isforest & !israngel) {
        stop('functional groups in `x$fg` not valid')
}
if(isforest){
        orgtype <- orgtype_f
}
if(israngel){
        orgtype <- orgtype_r
}
cvrclass <- c(0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 99)
# cvrclass <- c(0,1,2,3,4,5,6,7,8,9,10)  ### for FIA
cvrmidpt <- c(0, 0.001, 0.005, 0.015, 0.035, 0.075, 0.175, 0.375,
              0.625, 0.85, 0.975)
if (!all(x$cover %in% cvrclass)) {
        stop('cover classes in `x$cover` not valid')
}
depclass <- c(0,0.125*(2^(0:7)))
if (!all(x$cover %in% cvrclass)) {
        stop('depth classes in `x$depth` not valid (check units?)')
}


# pre-allocate a few vectors
x$volume <- x$cvalue <- x$nvalue <- x$predCN <- x$preddens <-
        x$mass <- x$predC <- x$predN <- x$type <- NA
# convert cover classes to midpoint percentage values
x$cover   <- mapvalues(x$cover, from=cvrclass, to=cvrmidpt, warn=F)
x$covercm <- x$cover * 1000      # convert plot cover to square cm
x$depth   <- x$depth * 2.54      # convert depth to cm
x$volume  <- x$depth * x$covercm # calc volume in cubic cm
x$mpid    <- paste0(x$pid, '_',
                    formatC(x$microquad,width=2,format='d',flag ='0'))
# assign organism type
x$type    <- mapvalues(x$fg, from=fg, to=orgtype, warn=F)
x         <- arrange(x, pid, microquad)

### check
if (!diff(range(table(x$fg))) < .Machine$double.eps ^ 0.5) {
        message('adding zero values for unobserved functional groups')
        ###   TODO   ###
}

###   calc biomass, C, N, CN from 2013 calibration data
# N differs by fxnl grp in calibrn set, so assign by fxnl grp means
grps <- ddply(cal, .(fg), summarize, meann=round(mean(n, na.rm=T),2))
crwk <- data.frame(news=levels(x$fg),# crosswalk old fxnl grps to new
                   olds=c('C','C','E','C','C','C',
                          'L','L','A','A','E'),
                   c=mean(cal$c, na.rm=T), n=NA)
crwk$n   <- grps$meann[match(crwk$olds, grps$fg)]   # match n%
x$cvalue <- crwk$c [match(x$fg, crwk$news)]         # assign c%
x$nvalue <- crwk$n [match(x$fg, crwk$news)]         # assign n%
# neg exp model to predict for implementation plots
f2 <- deriv3( ~ Const+a*exp(-b*depth), c('Const','a','b'),
              function(Const,a,b,depth) NULL)
m2 <- nls(dens~f2(Const,a,b,depth),data=cal,
          start = list(Const=0.5,a=0.01,b=0.5))
preddens    <- predict(m2, newdata=list(depth=x$depth))
x$preddens  <- preddens[1:length(preddens)] # predicted density (g/cm^3)
x$mass      <- x$preddens * x$volume      # mass      (g/microplot)
x$predC     <- x$mass * x$cvalue/100      # organic C (g/microplot)
x$predN     <- x$mass * x$nvalue/100      # total N   (g/microplot)
x$predCN    <- x$predC / x$predN          # CN ratio
rm(cal, grps, crwk, m2, f2, preddens)
### end pre-processing section ####

######################################################################
####    Aggregate to plot level    ####

### Aggregate for FXNL GRPS:
# SE function returns '0' instead of NA for zero-length vectors
`myse` <- function(a, ...) {
        if ( length(a) < 2 ){
                return(0L)
        } else {
                out <- sd(a, na.rm=T) / sqrt(length(na.omit(a))-1)
                out
        }
}
# mean and SE of growth forms within each plot...
tmp <- ddply(x, .(fg, pid), summarize,
             massp  = mean(mass*100, na.rm=T),# kg/ha
             masssd = myse(mass*100, na.rm=T),# kg/ha
             c      = mean(predC*100, na.rm=T), # kg/ha
             csd    = myse(predC*100, na.rm=T), # kg/ha
             n      = mean(predN*100, na.rm=T), # kg/ha
             nsd    = myse(predN*100, na.rm=T), # kg/ha
             vol    = mean(volume*0.1, na.rm=T), # m3/ha
             volsd  = myse(volume*0.1, na.rm=T), # m3/ha
             coverp = mean(cover*100,na.rm=T),   # %
             coversd= myse(cover*100, na.rm=T), # % per mp
             .progress='time' )
# ... then across all plots
tmpgf <- ddply(tmp, .(fg), summarize,
               mass   = mean(massp,  na.rm=T), # kg/ha
               masssd = mean(masssd, na.rm=T), # kg/ha
               c      = mean(c,      na.rm=T), # kg/ha
               csd    = mean(csd,    na.rm=T), # kg/ha
               n      = mean(n,      na.rm=T), # kg/ha
               nsd    = mean(nsd,    na.rm=T), # kg/ha
               vol    = mean(vol,    na.rm=T), # m3/ha
               volsd  = mean(volsd,  na.rm=T), # m3/ha
               cover  = mean(coverp, na.rm=T), # %
               coversd= mean(coversd,na.rm=T), # % per mp
               .progress='time' )
(tmpgf <- data.frame(fg=tmpgf[,1], round(tmpgf [,-1], 1)))
rm(tmp, myse)

### Aggregate for PLOTS:
# first, sum per microplot for ea type, then both moss/lich combined
tmp <- ddply(x, .(mpid, type), summarize,
             mp_mass = round(sum(mass,na.rm=TRUE),4))
tmp <- dcast(tmp, mpid ~ type, value.var = c('mp_mass') )
tmp[is.na(tmp)] <- 0
tmp$combo <- tmp$lich + tmp$moss + tmp$crust
x <- join(tmp, x, by=c('mpid'), type='full', match='all') ; rm(tmp)
x <- rename(x, c('moss'='mp_moss', 'lich'='mp_lich', # g/microplot
                 'crust'='mp_crust', 'combo'='mp_combo'))
x <- ddply(x,.(mpid), mutate,                 # sum *within* mp
           mp_c      = sum(predC, na.rm=TRUE), # g/microplot
           mp_n      = sum(predN, na.rm=TRUE), # g/microplot
           mp_vol    = sum(volume,na.rm=TRUE), # cm3/microplot
           mp_cover  = sum(cover, na.rm=TRUE)) # % per mp
# aggregate to plot-level
d <- ddply(x, .(pid), summarize,
           combo_mn = mean(mp_combo*100,na.rm=T),# kg/ha
           combo_sd = sd(mp_combo*100,  na.rm=T),# kg/ha
           cv       = combosd/combo,             # CV ratio
           lich_mn  = mean(mp_lich*100,na.rm=T), # kg/ha
           lich_sd  = sd(mp_lich*100,  na.rm=T), # kg/ha
           moss_mn  = mean(mp_moss*100,na.rm=T), # kg/ha
           moss_sd  =  sd(mp_moss*100, na.rm=T), # kg/ha
           crust_mn = mean(mp_crust*100,na.rm=T),# kg/ha
           crust_sd =  sd(mp_crust*100, na.rm=T),# kg/ha
           c_mn     = mean(mp_c*100,   na.rm=T), # kg/ha
           c_sd     =   sd(mp_c*100,   na.rm=T), # kg/ha
           n_mn     = mean(mp_n*100,   na.rm=T), # kg/ha
           n_sd     =   sd(mp_n*100,   na.rm=T), # kg/ha
           vol_mn   = mean(mp_vol*0.1, na.rm=T), # m3/ha
           vol_sd   =   sd(mp_vol*0.1, na.rm=T), # m3/ha
           cover_mn = mean(mp_cover,na.rm=T),    # %
           cover_sd =  sd(mp_cover, na.rm=T),    # %
           matdepth_mn= mean(depth,      na.rm=T), # cm
           lat     = head(na.omit(latdd),1),    # degrees
           lon     = head(na.omit(londd),1))    # degrees
d$fgr <- ddply(x[x$covercm != '0',,drop = T], .(pid), summarize,
               fgr=length(unique(fg)))$fgr      # richness
d    <- arrange(d, pid)
dcast(x, mpid ~ fg, value.var='mass', drop=F)
wide <- dcast(x, pid ~ fg, fun.aggregate=sum,value.var='mass',drop=F)
row.names(wide) <- wide[,1] ; wide <- wide[,-1]
d <- cbind(d, hill = renyi(wide, scales=c(1),hill=T))
tot <- data.frame(  # summaries of all 5 plots
        pid='Mean of all 5 plots',
        t(data.frame(mean=apply(d[,-1], 2, mean, na.rm=T))))
(summaries <- rbind(d, tot))

# write.csv(tmpgf, 'milton_summary_fg.csv', row.names=F)
# write.csv(summaries, 'milton_summary_plot.csv', row.names=F)
# write.csv(x, 'milton_microquads.csv', row.names=F)
#### end aggregate section ####

######################################################################

######################################################################
####    Plotting    ####
# require(ggplot2)    # for plotting NMS
# require(gridExtra)  # for arranging multiple ggplots


# # Biomass map (faceted by fxnl grp)
# tmp <- ddply(x, .(pid, fg), summarize, kgha=mean(mass*100, na.rm=T))
# tmp$lat  <- x$lat [match(tmp$pid, x$pid)]         # assign lat
# tmp$lon  <- x$lon [match(tmp$pid, x$pid)]         # assign lon
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
# p1 <- ggplot(d, aes(x=lon, y=lat, colour=combo)) +
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

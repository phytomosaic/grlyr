# grlyr

Ground Layer Estimation.


## What

Estimate biomass, carbon and nitrogen of moss and lichen ground layers as part of the US Forest Service's Forest Inventory and Analysis program. 



## Why

Ground layers (moss and lichen mats) are vital indicators of ecosystem health.  Evaluating status and trends of critical metrics (biomass, carbon content, nitrogen content) reveals much about the health and functioning of forests and rangelands.  A functional group approach (based on growth-forms and nutrient fixation capacity) can also reveal the specific contributions of key 'actors' in ground layers.  This approach complements other vegetation surveys at the same sampling locations.


## Installation

Install the package from github as follows:
```r
install.packages('devtools')
devtools::install_github('phytomosaic/grlyr')
```


## Further information

```r
require(grlyr)    # load the package
citation('grlyr') # please cite in publications
?est              # find a help file 
```


## Load data

Get the example dataset of estimation points from interior Alaska:
```r
data(est)
```


## Estimates

Calculate biomass, C, N values:
```r
x <- calc_biomass(est)
rm(est)
```


## Summaries

The expected level of summarization for USFS-FIA purposes is at the plot-level:
```r
s <- summary_plot(x)
```

...but you can also summarize by functional groups, either across all plots or within each:
```r
summary_fg(x)
summary_fg(x, eachplot=TRUE)
```


## Plot values faceted by groups

Distribution of plot values by functional group, with lines at the 25th, 50th, 75th percentiles:
```r
fg <- summary_fg(x, eachplot=TRUE)
fg$logmass <- log10(fg$mass+1)
plot_facet(fg, 'logmass', by='fg', type='hist', breaks=33,
           xlab='Biomass (kg ha)', ylab='Frequency')
```


## Mapping

Map biomass, C, N, and functional group richness in geographic space:
```r
s$lat <- x$lat[match(s$plot, x$plot)]  # match lat/lon to s
s$lon <- x$lon[match(s$plot, x$plot)]  # match lat/lon to s
par(mfrow=c(2,2))
plot_map(s, total_mn, main='Total biomass')
plot_map(s, c_mn, main='Moss biomass')
plot_map(s, n_mn, main='Lichen biomass')
plot_map(s, fgr, main='Functional grp richness')
```


Please contact `smithr2@oregonstate.edu` for questions or updates.

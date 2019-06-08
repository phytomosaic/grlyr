# grlyr

Ground Layer Estimation.


## What

Estimate biomass, carbon and nitrogen of moss and lichen ground layers as part of the US Forest Service's Forest Inventory and Analysis program. 



## Why

Terrestrial moss and lichen mats (**ground layers**) regulate the cycling of water and nutrients throughout landscapes, and provide critical forage and habitat for animals.  As such, evaluating status and trends of ground layer metrics (e.g., biomass, carbon content, nitrogen content, functional richness) can reveal much about the health and functioning of forests and rangelands.


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

Load the example dataset of estimation points from interior Alaska.
```r
data(est)
# est <- read.csv('est.csv', header=TRUE) # alternatively, read a csv
```


## Check data

First 5 column names must be "plot","microquad","fg","cover","depth".  Data must be in "long" format, where each row is a cover and depth measurement for a functional group in a microquad in a plot.
```r
check_dat(est)  # expect TRUE
```


## Estimates

Core function to calculate biomass, C, N values.
```r
x <- calc_biomass(est)
rm(est)
```


## Summaries

The expected level of summarization for USFS-FIA purposes is at the plot-level...
```r
s <- summary_plot(x)
```

...but you can also summarize by functional groups, either across all plots or within each.
```r
summary_fg(x)
summary_fg(x, eachplot=TRUE)
```


## Plot values faceted by groups

Distribution of plot-level values by functional group, with lines at the 25th, 50th, 75th percentiles.
```r
fg <- summary_fg(x, eachplot=TRUE)
fg$logmass <- log10(fg$mass+1)
plot_facet(fg, 'logmass', by='fg', type='hist', breaks=33,
           xlab='Biomass (kg ha)', ylab='Frequency')
```


## Mapping

Map biomass, C, N, and functional group richness in geographic space.
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

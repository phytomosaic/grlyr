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


## Load data

Get the example dataset from Milton Ranch (2016):
```r
require(grlyr)
data(milton)
x <- milton
rm(milton)
```


## Estimates

Calculate biomass, C, N values:
```r
head(x)  # TODO
```


## Mapping

Map biomass, C, N values in geographic space:
```r
head(x)  # TODO
```

Please contact `smithr2@oregonstate.edu` for questions or updates.

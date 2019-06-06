#' @name cal
#' @title Calibration data
#' @aliases cal
#' @docType data
#' @description
#' Calibration data using 150 destructive samples, used to build a
#'     calibration model for non-destructive estimation of ground
#'     layer biomass, carbon and nitrogen content.  Samples were
#'     collected in 2012 and 2013 from Oregon and Alaska, USA.
#'
#' @usage
#' data(cal)
#'
#' @format
#' A data.frame of 150 observations containing columns:\cr
#'     - \code{sampleid} unique identifier for each sample;\cr
#'     - \code{plotid} unique identifier for each sample location;\cr
#'     - \code{date} sampling date;\cr
#'     - \code{year} sampling year;\cr
#'     - \code{species} 6-digit abbreviation for each species;\cr
#'     - \code{fg} abbreviation for functional groups;\cr
#'     - \code{type} moss 'm' or lichen 'l' organism type;\cr
#'     - \code{ovendrymass} oven-dry mass, in grams;\cr
#'     - \code{depth} vertical depth, in centimeters;\cr
#'     - \code{area} horizontal area, in square centimeters;\cr
#'     - \code{c} total carbon content, as a percentage;\cr
#'     - \code{n} total nitrogen content, as a percentage;\cr
#'     - \code{vol} volume, in cubic centimeters;\cr
#'     - \code{dens} density, in grams per cubic centimeter;\cr
#'
#' @details
#' Destructive samples were collected to parameterize a nondestructive
#'      estimation model for the US Forest Service's Forest Inventory
#'      and Analysis program.  The specific FIA protocol is the
#'      'Ground Layer Indicator' (FIA 2018), first implemented as part
#'      of the Interior Alaska Inventory beginning in 2014.  The
#'      calibration model (a negative exponential function) is fully
#'      parameterized and described in Smith et al. (2015).  The
#'      simplified functional groups in this dataset must be
#'      crosswalked to their expanded versions prior to estimation.
#'
#' @references
#'
#' Smith, R.J., J.C. Benavides, S. Jovan, M. Amacher, and B. McCune.
#'      2015. A rapid method for landscape assessment of carbon
#'      storage and ecosystem function in moss and lichen ground
#'      layers. The Bryologist 118(1): 32–45.
#'
#' FIA [Forest Inventory and Analysis Program]. 2018. Field
#'      Instructions for the Annual Inventory of Alaska 2018:
#'      Supplement for Interior Alaska. USDA Forest Service, Pacific
#'      Northwest Research Station, Anchorage, Alaska.
#'
#' @keywords datasets
"cal"

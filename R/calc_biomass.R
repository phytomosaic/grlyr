#' @title Calculate biomass
#'
#' @description
#' Negative exponential model to calculate biomass while accounting
#'     for density variation with ground layer depth.
#'
#' @param x data.frame, minimally containing the columns
#'     \code{c("plot","microquad","fg","cover","depth")}.  Each row is
#'     an observation for each functional grp within a microquad,
#'     nested within transects > subplots > plots.
#'
#' @param ... further arguments (currently ignored).
#'
#' @return
#' A data.frame of same number of rows as `x`, with new columns:
#'    'mqid', 'type', 'predCN', 'predN', 'predC', 'mass', 'dens',
#'    'nvalue', 'cvalue', 'volume', 'covercm'.
#'
#' @details
#' Returns estimates of biomass, organic carbon, total nitrogen, and
#'      C:N ratio for each functional group observed per microquad.
#'      Equations follow Smith et al. (2015).  Example usage is in
#'      Smith et al. (2017).
#'
#' @examples
#' data(est)
#' x <- calc_biomass(est)
#' head(x)
#'
#' @references
#' Smith, R.J., J.C. Benavides, S. Jovan, M. Amacher, and B. McCune.
#'      2015. A rapid method for landscape assessment of carbon
#'      storage and ecosystem function in moss and lichen ground
#'      layers. The Bryologist 118(1): 32–45.
#'
#' Smith, R. J., S. Jovan, A. N. Gray, and B. McCune. 2017.
#'     Sensitivity of carbon stores in boreal forest moss mats -
#'     effects of vegetation, topography and climate. Plant and Soil
#'     421:31–42.
#'
#' @export
#' @rdname calc_biomass
`calc_biomass` <- function(x, ...){

        # load calibration data
        data(cal, envir = new.env())

        # check column names
        if (!check_dat(x)){
                stop('first 5 column names must be:
                 "plot"  "microquad"  "gf"  "cover"  "depth"
                     These columns must not have NA values.')
        }

        # pre-allocate a few vectors
        x$volume <- x$cvalue <- x$nvalue <-
                x$dens <- x$mass <- x$predC <- x$predN <-
                x$predCN <- x$type <- x$mqid <- NA

        # checks for functional groups
        fg_forest <- c('CC','CO',
                       'LF','LLFOL','LLFRU','LNFOL','LNFRU',
                       'MF','MN','MS','MT',
                       'VF','VS')
        orgtype_f <- c(rep('lich',2),rep('lich',5),
                       rep('moss',4),rep('moss',2))
        fg_cal_f  <- c('C','C','R','L','L','E','E','P','F','S','A',
                       'C','C')
        fg_rangel <- c('CBIND', 'CCYANO', 'CN', 'CO', 'CROCK','CSOIL',
                       'LF','LLFOL','LLFRU','LNFOL','LNFRU',
                       'MF','MN','MS','MT',
                       'VF','VS',
                       'MTL')
        orgtype_r <- c(rep('crust',6),rep('lich',5),
                       rep('moss',4),rep('moss',3))
        fg_cal_r  <- c(rep('C',4), fg_cal_f, 'A')
        isforest  <- all(x$fg %in% fg_forest)  # forestland protocol
        israngel  <- all(x$fg %in% fg_rangel)  # rangeland protocol
        if (!isforest & !israngel) {
                stop('functional groups in `x$fg` not valid')
        }
        orgtype <- if (isforest) orgtype_f else orgtype_r
        fg      <- if (isforest) fg_forest else fg_rangel
        fg_cal  <- if (isforest) fg_cal_f  else fg_cal_r

        # checks for cover classes
        cvr_fia <- c(0, 1, 2 ,3, 4, 5, 6, 7, 8, 9, 10)
        cvr_pct <- c(0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 99)
        cvr_mid <- c(0, 0.001, 0.005, 0.015, 0.035, 0.075, 0.175,
                     0.375, 0.625, 0.85, 0.975)
        is_cfia <- all(x$cover %in% cvr_fia)
        is_cpct <- all(x$cover %in% cvr_pct)
        is_cmid <- all(x$cover %in% cvr_mid)
        if (!is_cfia & !is_cpct & !is_cmid) {
                stop('cover classes in `x$cover` not valid')
        }
        cvrcls <- if (is_cfia) { cvr_fia } else {
                if (is_cpct) { cvr_pct } else {
                        if (is_cmid) { cvr_mid } else { NULL }}}

        # checks for depth classes
        dep_in <- c(0,0.125*(2^(0:7)))          # inch
        dep_cm <- c(0,0.3125*(2^(0:7)))         # cm
        dep_cc <- c(0,0.25,0.50,1,2,5,10,20,40) # cm
        dep_it <- 0:8                           # integer
        is_din <- all(x$depth %in% dep_in)
        is_dcm <- all(x$depth %in% dep_cm)
        is_dcc <- all(x$depth %in% dep_cc)
        is_dit <- all(x$depth %in% dep_it)
        if (!is_din & !is_dcm & !is_dcc & !is_dit) {
                stop('depth classes in `x$depth` not valid')
        }
        depcls <- if (is_din) { dep_in } else {
                if (is_dcm) { dep_cm } else {
                        if (is_dcc) { dep_cc } else {
                                if (is_dit) { dep_it } else {
                                        NULL
                                }}}}

        # checks for microquad unique identifier
        `f` <- function(xx) {
                formatC(xx, width=2, format = 'd', flag ='0')
        }
        if (all(sort(unique(x$microquad)) == c(5,10,15,20))) {
                x$mqid <- paste(f(x$plot),
                                # f(x$subp),
                                f(x$transect),
                                f(x$microquad),sep='_')
        } else {
                if (all(sort(unique(x$microquad)) == 1:32)) {
                        x$mqid <- paste0(x$plot, '_', f(x$microquad))
                } else {
                        stop('values in `x$microquad` not valid')
                }
        }

        # convert cover classes to midpoint percentage values
        x$cover  <- plyr::mapvalues(x$cover, from=cvrcls, to=cvr_mid,
                                    warn=F)
        x$covercm <- x$cover * 1000 # convert percent cover to sq cm

        # convert depth to midpoint cm values
        dep_mid <- c(0, 0.158750, 0.4490128, 0.8980256, 1.7960512,
                     3.5921024, 7.1842049, 14.3684098, 28.7368196)
        x$depthcm <- plyr::mapvalues(x$depth, from=depcls, to=dep_mid,
                                     warn=F)

        # calc volume in cubic cm
        x$volume <- x$depthcm * x$covercm

        # assign organism type
        x$type <- plyr::mapvalues(x$fg, from=fg, to=orgtype, warn=F)

        # order the data.frame by plot then microquad
        x <- plyr::arrange(x, plot, microquad)

        # negative exponential model from 2013 calibration data
        grps <- plyr::ddply(cal, plyr::.(fg), plyr::summarize,
                            meann=round(mean(n, na.rm=T),2))
        crwk <- data.frame(news=fg, olds=fg_cal,
                           c=mean(cal$c, na.rm=T), n=NA)
        crwk$n   <- grps$meann[match(crwk$olds, grps$fg)] # match n%
        x$cvalue <- crwk$c [match(x$fg, crwk$news)]       # assign c%
        x$nvalue <- crwk$n [match(x$fg, crwk$news)]       # assign n%

        # fit neg exp model manually (Smith et al. 2015)
        m    <- 0.0205
        a    <- 0.0512
        b    <- (-0.3448)
        dens <- m + a * exp(b * x$depthcm)
        x$dens   <- dens                  # density   (g/cm^3)
        x$mass   <- x$dens * x$volume     # mass      (g/microquad)
        x$predC  <- x$mass * x$cvalue/100 # organic C (g/microquad)
        x$predN  <- x$mass * x$nvalue/100 # total N   (g/microquad)
        x$predCN <- x$predC / x$predN     # C:N ratio
        class(x) <- c('grlyr', class(x))
        return(x)
}

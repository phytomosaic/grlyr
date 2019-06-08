#' @title Check data
#'
#' @description
#' Verifies proper data format before further processing.
#'
#' @param x data.frame containing the columns
#'     \code{ c("plot","microquad","fg","cover","depth") }.  Each row is
#'     an observation for each functional grp within a microquad,
#'     nested within transects > subplots > plots.
#'
#' @param ... further arguments (currently ignored).
#'
#' @return
#' Logical value.
#'
#' @examples
#' x <- matrix(NA, 5, 5)
#' dimnames(x)[[2]] <- c("plot","microquad","fg","cover","depth")
#' check_dat(x)
#' check_dat(data.frame(x))
#'
#' @export
#' @rdname check_dat
`check_dat` <- function(x, ...){
     isdf <- is.data.frame(x)
     nm   <- dimnames(x)[[2]]
     vec  <- c('plot', 'microquad', 'fg', 'cover', 'depth')
     all(isdf, nm[1:5] == vec)
}

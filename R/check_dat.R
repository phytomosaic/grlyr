#' @title Check data
#'
#' @description
#' Verifies proper data format before further processing.
#'
#' @param x data.frame containing the columns
#'              \code{c("pid","microquad","gf","cover","depth")}.
#'
#' @param ... further arguments passed to other functions.
#'
#' @return
#' Logical value.
#'
#' @examples
#' x <- matrix(NA, 5, 5)
#' dimnames(x)[[2]] <- c("pid","microquad","gf","cover","depth")
#' check_dat(x)
#' check_dat(data.frame(x))
#'
#' @export
#' @rdname check_dat
`check_dat` <- function(x, ...){
     isdf <- is.data.frame(x)
     nm   <- dimnames(x)[[2]]
     vec  <- c("pid","microquad","gf","cover","depth")
     all(isdf, nm[1:5] == vec)
}

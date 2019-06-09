#' @title Launch web app
#'
#' @description
#' Launch the web app to calculate biomass while accounting
#'     for density variation with ground layer depth.
#'
#' @return
#' Nothing, simply opens another window with the web app.
#'
#' @details
#' Web app allows an external user to upload a file of observations,
#'     then download an automatically generated file, without getting
#'     involved in any modeling or analysis.
#'
#' @examples
#' \dontrun{
#'     launch_app()
#' }
#'
#' @export
#' @rdname launch_app
`launch_app` <- function() {
     appDir <- system.file('shiny_examples', 'grlyr_web',
                           package = 'grlyr')
     if (appDir == '') {
          stop('Could not find example directory.
               Try re-installing `grlyr`.', call. = FALSE)
     }
     shiny::runApp(appDir, display.mode = 'normal')
}

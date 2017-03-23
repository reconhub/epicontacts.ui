#'
#' Web interface for epicontacts
#'
#' This function opens up an application in a web browser for an interactive use
#' of RECON's epicontacts package.
#'
#' @seealso
#' The epicontacts package: \url{http://www.repidemicsconsortium.org/epicontacts/}
#'
#'
#' @import shiny
#'
#' @export
epicontacts_ui <- function(){
  shiny::runApp(system.file("shiny", package="epicontacts.ui"))
  invisible()
}

#' Start the CyAN app
#'
#' Launch the CyAN app in a browser
#'
#' @export
#'

run_CyAN <- function() {

  app_dir <- system.file("shiny", "cyan-app", package = "CyAN")
  if(app_dir == "") {
    stop("Couldn't find app directory, try reinstalling the package")
  }

  shiny::runApp(app_dir, launch.browser = TRUE)

}



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

  shiny::runApp(appDir, launch.browser = TRUE)

}

#' Return a null value for an empty character string
#'
#' If x is a blank character vector, return null, otherwise make sure x is numeric
#'
#' @param x a blank character string or number as text
#'
#' @return either NULL or a number
#'

null_if_blank_as_num <- function(x) {
  if(x == "") {
    return(NULL)
  } else {
    return(as.numeric(x))
  }
}



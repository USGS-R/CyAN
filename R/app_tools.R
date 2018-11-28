
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



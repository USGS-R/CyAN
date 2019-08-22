#' Find results with a particular flag
#'
#' Check the QC_FLAGS table to find results that have been flagged with
#' the given flag code
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#' @param flag_code the flag code of interest
#' @param collect a logical indicating whether the query will be pulled into
#' a local tibble using dbplyr::collect. If you are planning on doing further
#' selection or filtering, you may want not want to collect until you're ready
#'
#' @return a table of all results that have been flagged with the specified flag code
#'
#' @importFrom magrittr %>%
#'
#' @export

find_flagged_data <- function(cyan_connection, flag_code, collect = TRUE) {

  FLAG_CODE <- RESULT_ID <- ".dplyr.var"

  flags <- dplyr::tbl(cyan_connection, "QC_FLAGS") %>%
    dplyr::filter(FLAG_CODE == flag_code) %>%
    dplyr::pull(RESULT_ID)

  result <- dplyr::tbl(cyan_connection, "RESULT") %>%
    dplyr::filter(RESULT_ID %in% flags)
  activity <- dplyr::tbl(cyan_connection, "ACTIVITY")
  location <- dplyr::tbl(cyan_connection, "LOCATION")

  output <- result %>%
    inner_join(activity) %>%
    inner_join(location)

  if(collect)
    output <- collect(output)

  return(output)
}

#' Find results with a particular flag
#'
#' Check the QC_FLAGS table to find results that have been flagged with
#' the given flag code
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#' @param flag_code the flag code of interest
#'
#' @return a vector of result identifiers
#'
#' @importFrom magrittr %>%
#'
#' @export

find_flagged <- function(cyan_connection, flag_code) {

  FLAG_CODE <- RESULT_ID <- ".dplyr.var"

  flags <- dplyr::tbl(cyan_connection, "QC_FLAGS") %>%
    dplyr::filter(FLAG_CODE == flag_code) %>%
    dplyr::pull(RESULT_ID)

  return(flags)
}

#' Apply a given flag to results
#'
#' Add flags to the QCFLAGS table as defined in the FLAG_KEY table to a given
#' vector of RESULT_IDs
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#' @param flag_code the flag code to apply
#' @param initials initials to tag the flag with
#' @param results the results to apply the flag to
#'
#' @return a logical indicating whether the flags were succesfully written
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

apply_flags <- function(cyan_connection, flag_code, initials, results) {

  FLAG_CODE <- FLAG_ID <- ".dplyr.var"

  defined_flags <- dplyr::tbl(cyan_connection, "FLAG_KEY") %>%
    pull(FLAG_CODE)

  if(!(flag_code %in% defined_flags))
    stop(paste(flag_code, "not defined in the database"))

  ids <- dplyr::tbl(cyan_connection, "QC_FLAGS") %>%
    dplyr::pull(FLAG_ID)
  if(length(ids) == 0) {
    max_key <- 0
  } else {
    max_key <- max(ids)
  }

  to_write <- data.frame(
    FLAG_ID = 1:length(results) + max_key,
    RESULT_ID = results,
    FLAG_CODE = rep(flag_code, length(results)),
    INITIALS = rep(initials, length(results)),
    DATETIME_APPLIED = as.character(rep(Sys.time(), length(results)))
  )

  written <- DBI::dbWriteTable(cyan_connection, "QC_FLAGS", to_write, append = TRUE)
  return(written)

}

#' Remove flags from the QC_FLAGS table
#'
#' Given a flag_code and a vector of result identifiers, remove all the applicable
#' rows from the QC_FLAGS table
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan()}
#' @param flag_code the flag code to remove
#' @param results the result identifiers to remove the given flag from
#'
#' @return a numeric vector indicating the number of flags deleted for each result
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

remove_flags <- function(cyan_connection, flag_code, results) {

  affected <- vector(mode = "numeric", length = length(results))
  for(i in seq_along(results)) {
    statement <- paste("DELETE FROM QC_FLAGS WHERE FLAG_CODE = '",
                       flag_code, "' ",
                       "AND RESULT_ID = ",
                       results[i], ";",
                       sep = "")
    affected[i] <- DBI::dbExecute(cyan_connection, statement)
  }

  return(affected)

}

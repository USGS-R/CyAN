#' Connect to the CyAN database
#'
#' Given a file path, connect to the CyAN database. The database can be
#' downloaded manually from the FTP site, or using the download function
#'
#' @param path the full path to the database
#'
#' @return a SQLite database connection
#'
#' @export

connect_cyan <- function(path) {

  if(!file.exists(path)) {
    stop("file path doesn't exist, or isn't available")
  }

  cyan_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  table_names <- DBI::dbListTables(cyan_connection)

  if(!(all(c("ACTIVITY", "LOCATION", "RESULT") %in% table_names))) {
    stop("not a valid copy of the cyan database")
  }

  return(cyan_connection)

}

#' Generate an index of locations and parameters from the database
#'
#' Based on the CyAN database connection, generate an index of all unique combinations
#' of locations and parameters that have been measured there
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#'
#' @return a data frame of all unique combinations of location and parameter
#'
#' @importFrom magrittr %>%

generate_location_index <- function(cyan_connection) {

  LOCATION_ID <- LOCATION_NAME <- LATITUDE <- LONGITUDE <-
    ACTIVITY_ID <- PARAMETER_ID <- ".dplyr.var"

  location <- dplyr::tbl(cyan_connection, "LOCATION") %>%
    dplyr::select(LOCATION_ID, LOCATION_NAME, LATITUDE, LONGITUDE)
  activity <- dplyr::tbl(cyan_connection, "ACTIVITY") %>%
    dplyr::select(LOCATION_ID, ACTIVITY_ID)
  result <- dplyr::tbl(cyan_connection, "RESULT") %>%
    dplyr::select(ACTIVITY_ID, PARAMETER_ID)

  location_index <- location %>%
    dplyr::inner_join(activity, by = "LOCATION_ID") %>%
    dplyr::inner_join(result, by = "ACTIVITY_ID") %>%
    dplyr::select(LOCATION_NAME, LONGITUDE, LATITUDE, PARAMETER_ID) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  return(location_index)

}

#' Generate a list of parameter identifiers and their short names
#'
#' Lookup table relating each arbitrary parameter identifier to the short
#' version of the parameter name
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#'
#' @return a data frame of parameter identifiers and their associated
#' short names
#'
#' @importFrom magrittr %>%

generate_parameter_index <- function(cyan_connection) {

  PARAMETER_ID <- SHORT_NAME <- ".dplyr.var"

  parameter_index <- dplyr::tbl(cyan_connection, "PARAMETER") %>%
    dplyr::select(PARAMETER_ID, SHORT_NAME) %>%
    dplyr::collect()

  return(parameter_index)

}

#' Get data from CyAN
#'
#' Basic data query from the CyAN database based on a number of parameters,
#' used to drive the data download from the map screen of the shiny app. Can also
#' be used with the plotting functions
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#'
#' @param collect a logical indicating whether the query will be pulled into
#' a local tibble using dbplyr::collect. If you are planning on doing further
#' selection or filtering, you may want not want to collect until you're ready
#'
#' @param north_latitude,south_latitude numbers indicating the northern most
#' and sothern most latitude that will be included in the query. Latitude
#' should be given as a positive number of degrees north of the equator.
#'
#' @param west_longitude,east_longitude nunbers indicating the western most
#' and eastern most latitude that will be included in the query. Longitude
#' should be given as a negative number of decimal degrees west of the prime
#' meridian.
#'
#' @param start_date,end_date dates can be given as character strings in the
#' form "yyyy-mm-dd" or as Date objects
#'
#' @param parameters a character vector of parameter names that will be
#' returned in the query
#'
#' @param minimum_tier a number between 1.0 and 4.0 indicating the minimum
#' data tier that will be returned in the query
#'
#' @param states a character vector of the two-character state abbreviations
#' the query will included
#'
#' @return if collect is FALSE, the query will be generated, but not collected.
#' See the documentation on \code{collect} for details. Otherwise, if collect
#' is TRUE, the query will be pulled into memory and returned as a tibble. If
#' your query is complicated, or will return a lot of rows, it could take a
#' long time - minutes.
#'
#' @importFrom magrittr %>%
#'
#' @export

get_cyan_data <- function(cyan_connection, collect = FALSE,
                          north_latitude = NULL, south_latitude = NULL,
                          west_longitude = NULL, east_longitude = NULL,
                          start_date = NULL, end_date = NULL,
                          parameters = NULL,
                          minimum_tier = NULL,
                          states  = NULL) {

  LOCATION_ID <- LATITUDE.LOCATION <- LONGITUDE.LOCATION <- STARTDATE <-
    STARTTIME <- ENDDATE <- ENDTIME <- TIMEZONE <- SAMPLE_TYPE <-
    DEPTH <- DEPTH_UNIT <- VALUE <- FLAG <- PARAMETER_ID.PARAMETER <-
    LOCALTZ <- PARAMETER_NAME <- UNITS <- ACTIVITY_ID <- RESULT_ID <-
    TIER <- METHOD_ID <- NOTE <- LATITUDE <- LONGITUDE <- PARAMETER_ID <-
    STATECODE <- ".dplyr.var"

  location <- dplyr::tbl(cyan_connection, "LOCATION")
  activity <- dplyr::tbl(cyan_connection, "ACTIVITY")
  result <- dplyr::tbl(cyan_connection, "RESULT")
  parameter <- dplyr::tbl(cyan_connection, "PARAMETER")
  method <- dplyr::tbl(cyan_connection, "METHOD")

  output <- location %>%
    dplyr::inner_join(activity, by = "LOCATION_ID", suffix = c(".LOCATION", ".ACTIVITY")) %>%
    dplyr::inner_join(result, by = "ACTIVITY_ID", suffix = c(".ACTIVITY", ".RESULT")) %>%
    dplyr::inner_join(parameter, by = "PARAMETER_ID", suffix = c(".RESULT", ".PARAMETER")) %>%
    dplyr::inner_join(method, by = "METHOD_ID", suffix = c(".PARAMETER", ".METHOD")) %>%
    dplyr::select(LOCATION_ID, LATITUDE.LOCATION, LONGITUDE.LOCATION, STARTDATE, STARTTIME,
           ENDDATE, ENDTIME, TIMEZONE, SAMPLE_TYPE, DEPTH, DEPTH_UNIT,
           VALUE, FLAG, PARAMETER_ID.PARAMETER, LOCALTZ, PARAMETER_NAME, UNITS,
           ACTIVITY_ID, RESULT_ID, TIER, METHOD_ID, NOTE) %>%
    dplyr::rename(LATITUDE = LATITUDE.LOCATION, LONGITUDE = LONGITUDE.LOCATION,
           PARAMETER_ID = PARAMETER_ID.PARAMETER)


  if(!(is.null(north_latitude))) {

    if(!is.numeric(north_latitude)) {
      stop("north_latitude should be a number, or left as NULL")
    }
    if(north_latitude < 24.54409) {
      warning("given north_latitude is south of the contiguous 48 states")
    }

    output <-  dplyr::filter(output, LATITUDE <= north_latitude)

  }
  if(!(is.null(south_latitude))) {

    if(!is.numeric(south_latitude)) {
      stop("south_latitude should be a number, or left as NULL")
    }
    if(north_latitude > 49.384472) {
      warning("given south_latitude is north of the contiguous 48 states")
    }

    output <-  dplyr::filter(output, LATITUDE >= south_latitude)

  }
  if(!(is.null(west_longitude))) {

    if(!is.numeric(west_longitude)) {
      stop("west_longitude should be a number, or left as NULL")
    }
    if(west_longitude > -66.949778) {
      warning("given west_longitude is east of the contiguous 48 states")
    }

    output <- dplyr::filter(output, LONGITUDE >= west_longitude)

  }
  if(!(is.null(east_longitude))) {

    if(!is.numeric(east_longitude)) {
      stop("east_longitude should be a number, or left as NULL")
    }
    if(east_longitude < -124.667222) {
      warning("given east_longitude is west of the contiguous 48 states")
    }

    output <- dplyr::filter(output, LONGITUDE <= east_longitude)

  }
  if(!(is.null(start_date))) {

    if(class(start_date) != "Date") {
      if(is.na(as.Date(start_date, format="%Y-%m-%d"))) {
        stop("start_date isn't a date or a character string like yyyy-mm-dd")
      }
    }

    output <- dplyr::filter(output, STARTDATE >= start_date)

  }
  if(!(is.null(end_date))) {

    if(class(end_date) != "Date") {
      if(is.na(as.Date(end_date, format="%Y-%m-%d"))) {
        stop("end_date isn't a date or a character string like yyyy-mm-dd")
      }
    }

    output <- dplyr::filter(output, STARTDATE <= end_date)

  }
  if(!(is.null(parameters))) {
    output <- dplyr::filter(output, PARAMETER_ID %in% parameters)
  }
  if(!(is.null(minimum_tier))) {
    output <- dplyr::filter(output, TIER >= minimum_tier)
  }
  if(!(is.null(states))) {
    output <- dplyr::filter(output, STATECODE %in% states)
  }

  if(collect) {
    message("Collecting output")
    output <- dplyr::collect(output)
  }

  return(output)

}


#' Find simaltaneous measurements of two parameters
#'
#' Find sampling activities that have associated measurements of two parameters
#' of interest - helpful for bivariate plotting/ investigating the relationship
#' between two parameters.
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#'
#' @param parameter_1,parameter_2 CyAN parameter codes for the two parameters
#' of interest
#'
#' @param collect a logical indicating whether the query will be pulled into
#' a local tibble using dbplyr::collect. If you are planning on doing further
#' selection or filtering, you may want not want to collect until you're ready
#'
#' @param north_latitude,south_latitude numbers indicating the northern most
#' and sothern most latitude that will be included in the query. Latitude
#' should be given as a positive number of degrees north of the equator.
#'
#' @param west_longitude,east_longitude nunbers indicating the western most
#' and eastern most latitude that will be included in the query. Longitude
#' should be given as a negative number of decimal degrees west of the prime
#' meridian.
#'
#' @param start_date,end_date dates can be given as character strings in the
#' form "yyyy-mm-dd" or as Date objects
#'
#' @importFrom magrittr %>%
#'
#' @export

get_bivariate <- function(cyan_connection, parameter_1, parameter_2,
                          collect = FALSE,
                          north_latitude = NULL, south_latitude = NULL,
                          west_longitude = NULL, east_longitude = NULL,
                          start_date = NULL, end_date = NULL) {

  all_data <- get_cyan_data(cyan_connection, north_latitude = north_latitude,
                            south_latitude = south_latitude,
                            east_longitude = east_longitude,
                            west_longitude = west_longitude,
                            start_date = start_date, end_date = end_date,
                            parameters = c(parameter_1, parameter_2))
  parameter_1_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_1)
  parameter_2_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_2)

  plot_data <- dplyr::inner_join(parameter_1_data, parameter_2_data,
                          by = c("ACTIVITY_ID", "DEPTH", "DEPTH_UNIT"),
                          suffix = c(".1", ".2"))

  if(collect)
    plot_data <- dplyr::collect(plot_data)

  return(plot_data)

}

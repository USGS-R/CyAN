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
#' @param years numeric vector of years that will be included in the query
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
                          years = NULL,
                          parameters = NULL,
                          minimum_tier = NULL,
                          states  = NULL) {

  # LOCATION_ID <- LATITUDE.LOCATION <- LONGITUDE.LOCATION <- STARTDATE <-
  #   STARTTIME <- ENDDATE <- ENDTIME <- TIMEZONE <- SAMPLE_TYPE <-
  #   DEPTH <- DEPTH_UNIT <- VALUE <- FLAG <- PARAMETER_ID.PARAMETER <-
  #   LOCALTZ <- PARAMETER_NAME <- UNITS <- ACTIVITY_ID <- RESULT_ID <-
  #   TIER <- METHOD_ID <- NOTE <- LATITUDE <- LONGITUDE <- PARAMETER_ID <-
  #   STATECODE <- ".dplyr.var"

  location <- dplyr::tbl(cyan_connection, "LOCATION") %>%
    dplyr::select(LOCATION_ID, LATITUDE, LONGITUDE, LOCATION_NAME, LOCALTZ)
  activity <- dplyr::tbl(cyan_connection, "ACTIVITY") %>%
    dplyr::select(ACTIVITY_ID, LOCATION_ID, STARTDATE, STARTTIME, ENDDATE, ENDTIME,
           TIMEZONE, SAMPLE_TYPE) %>%
    dplyr::mutate(YEAR = as.numeric(STRFTIME('%Y', STARTDATE)))
  result <- dplyr::tbl(cyan_connection, "RESULT") %>%
    dplyr::select(RESULT_ID, ACTIVITY_ID, VALUE, FLAG, PARAMETER_ID, METHOD_ID, DEPTH,
           DEPTH_UNIT, TIER)
  parameter <- dplyr::tbl(cyan_connection, "PARAMETER") %>%
    dplyr::select(PARAMETER_ID, PARAMETER_NAME, UNITS)
  method <- dplyr::tbl(cyan_connection, "METHOD") %>%
    dplyr::select(METHOD_ID, NOTE)

  if(!(is.null(north_latitude))) {

    if(!is.numeric(north_latitude)) {
      stop("north_latitude should be a number, or left as NULL")
    }
    if(north_latitude < 24.54409) {
      warning("given north_latitude is south of the contiguous 48 states")
    }

    location <- dplyr::filter(location, LATITUDE <= north_latitude)

  }
  if(!(is.null(south_latitude))) {

    if(!is.numeric(south_latitude)) {
      stop("south_latitude should be a number, or left as NULL")
    }
    if(north_latitude > 49.384472) {
      warning("given south_latitude is north of the contiguous 48 states")
    }

    location <- dplyr::filter(location, LATITUDE >= south_latitude)

  }
  if(!(is.null(west_longitude))) {

    if(!is.numeric(west_longitude)) {
      stop("west_longitude should be a number, or left as NULL")
    }
    if(west_longitude > -66.949778) {
      warning("given west_longitude is east of the contiguous 48 states")
    }

    location <- dplyr::filter(location, LONGITUDE >= west_longitude)

  }
  if(!(is.null(east_longitude))) {

    if(!is.numeric(east_longitude)) {
      stop("east_longitude should be a number, or left as NULL")
    }
    if(east_longitude < -124.667222) {
      warning("given east_longitude is west of the contiguous 48 states")
    }

    location <- dplyr::filter(location, LONGITUDE <= east_longitude)

  }
  if(!(is.null(years))) {

    if(!is.numeric(years)) {
      stop("years should be a vector of integers")
    }

    activity <- dplyr::filter(activity, YEAR %in% years)

  }
  if(!(is.null(parameters))) {
    result <- dplyr::filter(result, PARAMETER_ID %in% parameters)
  }
  if(!(is.null(minimum_tier))) {
    result <- dplyr::filter(result, TIER >= minimum_tier)
  }
  if(!(is.null(states))) {
    location <- dplyr::filter(location, STATECODE %in% states)
  }

  if(collect) {
    message("Collecting output")
    location <- dplyr::collect(location)
    activity <- dplyr::collect(activity)
    result <- dplyr::collect(result)
    parameter <- dplyr::collect(parameter)
    method <- dplyr::collect(method)
  }

  output <- location %>%
    dplyr::inner_join(activity, by = "LOCATION_ID", suffix = c(".LOCATION", ".ACTIVITY")) %>%
    dplyr::inner_join(result, by = "ACTIVITY_ID", suffix = c(".ACTIVITY", ".RESULT")) %>%
    dplyr::inner_join(parameter, by = "PARAMETER_ID", suffix = c(".RESULT", ".PARAMETER")) %>%
    dplyr::inner_join(method, by = "METHOD_ID", suffix = c(".PARAMETER", ".METHOD")) %>%
    dplyr::select(LOCATION_ID, LATITUDE, LONGITUDE, STARTDATE, STARTTIME,
                  ENDDATE, ENDTIME, TIMEZONE, SAMPLE_TYPE, DEPTH, DEPTH_UNIT,
                  VALUE, FLAG, PARAMETER_ID, LOCALTZ, PARAMETER_NAME, UNITS,
                  ACTIVITY_ID, RESULT_ID, TIER, METHOD_ID, NOTE)

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
##' @return if collect is FALSE, the query will be generated, but not collected.
#' See the documentation on \code{collect} for details. Otherwise, if collect
#' is TRUE, the query will be pulled into memory and returned as a tibble.
#' Returns a wide data frame, with a shared activity, depth, and depth_unit
#' columns, and all other columns suffixed with .1 or .2 for the two parameters.
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

#' Find results with a particular flag
#'
#' Check the QCFLAGS table to find results that have been flagged with
#' the given flag code
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan}
#'
#' @param flag_code the flag code of interest
#'
#' @return a vector of result_ids with the given flag
#'
#' @importFrom magrittr %>%
#'
#' @export

find_flagged <- function(cyan_connection, flag_code) {

  FLAG_CODE <- ".dplyr.var"

  flags <- dplyr::tbl(cyan_connection, "QCFLAGS") %>%
    dplyr::filter(FLAG_CODE == flag_code) %>%
    dplyr::pull(RESULT_ID)

  return(flags)

}

#' Add a column of GMT time to a cyan data query
#'
#' Find GMT time given the sample time and local time zone
#'
#' @param cyan_data a data frame from \code{get_cyan_data} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with an additional character column for time in GMT
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

add_GMT_time <- function(cyan_data) {

  output <- cyan_data %>%
    dplyr::mutate(datetime = paste(STARTDATE, STARTTIME))
  gmt_time <- vector(length = nrow(output))
  for(i in 1:nrow(output)) {
    gmt_time[i] <- as.character(lubridate::ymd_hms(output$datetime[i],
                                                   tz = output$TIMEZONE[i]), tz = "GMT")
  }
  #If any timezones are blank, don't output a GMT time
  gmt_time[output$TIMEZONE == ""] <- NA
  output$TIME_GMT <- gmt_time
  output <- dplyr::select(output, -datetime)
  return(output)

}

#' Add a logical column indicating whether the sample was taken during solar noon
#'
#' Solar noon is defined as 1000 - 1400, extended solar noon is defined as
#' 0900 - 1500
#'
#' @param cyan_data a data frame from \code{get_cyan_data} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with two additional logical columns, solar_noon indicating
#' if the sample was taken during solar noon and ext_solar_noon indicating if
#' the sample was taken during extended solar noon.
#'
#' @export

add_solar_noon <- function(cyan_data) {

  output <- dplyr::mutate(cyan_data, datetime = paste(STARTDATE, STARTTIME))

  hour <- vector(length = nrow(output))
  for(i in 1:nrow(cyan_data)) {
    hour[i] <- as.character(lubridate::ymd_hms(output$datetime[i],
                                               tz = output$TIMEZONE[i]),
                            tz = output$LOCALTZ[i], format = "%H")
  }
  hour <- as.numeric(hour)
  solar_noon <- hour %in% 10:13
  ext_solar_noon <- hour %in% 9:14
  solar_noon[output$TIMEZONE == "" | output$LOCALTZ == ""] <- NA
  ext_solar_noon[output$TIMEZONE == "" | output$LOCALTZ == ""] <- NA

  output$solar_noon <- solar_noon
  output$ext_solar_noon <- ext_solar_noon

  output <- dplyr::select(output, -datetime)
  return(output)

}

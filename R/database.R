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

  if(!(all(c("ACTIVITY", "LOCATION", "RESULT", "PARAMETER", "METHOD") %in% table_names))) {
    stop("not a valid database")
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
#'
#' @export

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
#'
#' @export

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

  LOCATION_ID <- LOCATION_NAME <- LATITUDE <- LONGITUDE <- DATUM <- STATE_CODE <-
    HUC <- START_DATE <- START_TIME <- END_DATE <- END_TIME <- TZ <-
    COLLECTION_METHOD_ID <- SAMPLE_TYPE_CODE <- ACTIVITY_DEPTH <-
    ACTIVITY_DEPTH_UNIT <- ACTIVITY_TOP_DEPTH <- ACTIVITY_TOP_DEPTH_UNIT <-
    ACTIVITY_BOTTOM_DEPTH <- ACTIVITY_BOTTOM_DEPTH_UNIT <-
    ACTIVITY_DEPTH_REFERENCE <- ACTIVITY_COMMENT <- RESULT_ID <- ACTIVITY_ID <-
    PARAMETER_ID <- METHOD_ID <- RESULT_DEPTH <- RESULT_DEPTH_UNIT <-
    RESULT_DEPTH_REFERENCE <- QUALIFIER <- RESULT_VALUE <- DETECTION_LIMIT_VALUE <-
    DETECTION_LIMIT_UNIT <- DETECTION_LIMIT_TYPE <- TIER <- CROSSWALK_ID <-
    PARAMETER_NAME <- UNITS <- WQP_METHOD_IDENTIFIER <- WQP_METHOD_CONTEXT <-
    WQP_METHOD_NAME <- WQP_METHOD_DESCRIPTION <- WQP_COLLECTION_METHOD_ID <-
    WQP_COLLECTION_METHOD_CONTEXT <- WQP_COLLECTION_METHOD_NAME <- LOCATION_TYPE <-
    LOCAL_TZ <- STRFTIME <- SAMPLE_TYPE_DEFINITION <- YEAR <- ".dplyr.var"

  location <- dplyr::tbl(cyan_connection, "LOCATION") %>%
    dplyr::select(LOCATION_ID, LOCATION_TYPE, LATITUDE, LONGITUDE, DATUM, STATE_CODE, HUC,
                  LOCATION_NAME, LOCAL_TZ)

  activity <- dplyr::tbl(cyan_connection, "ACTIVITY") %>%
    dplyr::select(ACTIVITY_ID, START_DATE, START_TIME, END_DATE, END_TIME, TZ,
                  LOCATION_ID, COLLECTION_METHOD_ID, SAMPLE_TYPE_CODE, ACTIVITY_DEPTH,
                  ACTIVITY_DEPTH_UNIT, ACTIVITY_TOP_DEPTH, ACTIVITY_TOP_DEPTH_UNIT,
                  ACTIVITY_BOTTOM_DEPTH, ACTIVITY_BOTTOM_DEPTH_UNIT,
                  ACTIVITY_DEPTH_REFERENCE, ACTIVITY_COMMENT) %>%
    dplyr::mutate(YEAR = as.numeric(STRFTIME('%Y', START_DATE)))

  result <- dplyr::tbl(cyan_connection, "RESULT") %>%
    dplyr::select(RESULT_ID, ACTIVITY_ID, PARAMETER_ID, METHOD_ID, RESULT_DEPTH,
                  RESULT_DEPTH_UNIT, RESULT_DEPTH_REFERENCE, QUALIFIER,
                  RESULT_VALUE, DETECTION_LIMIT_VALUE, DETECTION_LIMIT_UNIT,
                  DETECTION_LIMIT_TYPE, CROSSWALK_ID)

  parameter <- dplyr::tbl(cyan_connection, "PARAMETER") %>%
    dplyr::select(PARAMETER_ID, PARAMETER_NAME, UNITS)

  method <- dplyr::tbl(cyan_connection, "METHOD") %>%
    dplyr::select(METHOD_ID, WQP_METHOD_IDENTIFIER, WQP_METHOD_CONTEXT, WQP_METHOD_NAME,
                  WQP_METHOD_DESCRIPTION)

  sample_type <- dplyr::tbl(cyan_connection, "SAMPLE_TYPE") %>%
    dplyr::select(SAMPLE_TYPE_CODE, SAMPLE_TYPE_DEFINITION)

  collection_method <- dplyr::tbl(cyan_connection, "COLLECTION_METHOD") %>%
    dplyr::select(COLLECTION_METHOD_ID, WQP_COLLECTION_METHOD_ID,
                  WQP_COLLECTION_METHOD_CONTEXT, WQP_COLLECTION_METHOD_NAME)

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
    if(south_latitude > 49.384472) {
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
    valid_parms <- parameter %>% dplyr::pull(PARAMETER_ID)
    if(!all(parameters %in% valid_parms)) {
      invalid <- paste(parameters[!(parameters %in% valid_parms)], collapse = " ")
      stop(paste(invalid, "not valid parameter_ids"))
    }
    result <- dplyr::filter(result, PARAMETER_ID %in% parameters)
  }
  if(!(is.null(minimum_tier))) {
    if(!is.numeric(minimum_tier)) {
      stop("minimum tier should be a number between 5.0 and 1.0")
    }
    if(minimum_tier < 1.0) {
      stop("minimum tier number is 1.0")
    }
    result <- dplyr::filter(result, TIER >= minimum_tier)
  }
  if(!(is.null(states))) {
    if(!all(states %in% datasets::state.abb)) {
      invalid_states <- states[!(states %in% state.abb)]
      stop(paste(invalid_states, "not valid state abbreviations"))
    }
    location <- dplyr::filter(location, STATE_CODE %in% states)
  }

  if(collect) {
    message("Collecting output")
    location <- dplyr::collect(location)
    activity <- dplyr::collect(activity)
    result <- dplyr::collect(result)
    parameter <- dplyr::collect(parameter)
    method <- dplyr::collect(method)
    sample_type <- dplyr::collect(sample_type)
    collection_method <- dplyr::collect(collection_method)
  }

  output <- location %>%
    dplyr::inner_join(activity, by = "LOCATION_ID", suffix = c(".LOCATION", ".ACTIVITY")) %>%
    dplyr::inner_join(result, by = "ACTIVITY_ID", suffix = c(".ACTIVITY", ".RESULT")) %>%
    dplyr::inner_join(parameter, by = "PARAMETER_ID", suffix = c(".RESULT", ".PARAMETER")) %>%
    dplyr::inner_join(method, by = "METHOD_ID", suffix = c(".PARAMETER", ".METHOD")) %>%
    dplyr::inner_join(sample_type, by = "SAMPLE_TYPE_CODE",
                      suffix = c(".METHOD", ".SAMPLE_TYPE")) %>%
    dplyr::inner_join(collection_method, by = "COLLECTION_METHOD_ID",
                      suffix = c(".SAMPLE_TYPE", "COLLECTION_METHOD"))

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
#' @param years numeric vector of years that will be included in the query
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
                          years = NULL) {

  PARAMETER_ID <- ".dplyr.var"

  all_data <- get_cyan_data(cyan_connection, north_latitude = north_latitude,
                            south_latitude = south_latitude,
                            east_longitude = east_longitude,
                            west_longitude = west_longitude,
                            years = years,
                            parameters = c(parameter_1, parameter_2))
  parameter_1_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_1)
  parameter_2_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_2)

  plot_data <- dplyr::inner_join(parameter_1_data, parameter_2_data,
                          by = "ACTIVITY_ID",
                          suffix = c(".1", ".2"))

  if(collect)
    plot_data <- dplyr::collect(plot_data)

  return(plot_data)

}

#' Find results with a particular flag
#'
#' Check the QC_FLAGS table to find results that have been flagged with
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

  FLAG_CODE <- RESULT_ID <- ".dplyr.var"

  flags <- dplyr::tbl(cyan_connection, "QC_FLAGS") %>%
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

  START_DATE <- START_TIME <- datetime <- ".dplyr.var"

  output <- cyan_data %>%
    dplyr::mutate(datetime = paste(START_DATE, START_TIME))

  #Sometimes 3 letter time zone abbreviations cause issues
  timezone <- output$TZ
  state <- output$STATE_CODE
  timezone[timezone %in% c("EST", "EDT")] <- "America/New_York"
  timezone[timezone %in% c("CST", "CDT")] <- "America/Chicago"
  timezone[timezone == "MST" & state == "AZ"] <- "America/Phoenix"
  timezone[timezone %in% c("MST", "MDT")] <- "America/Denver"
  timezone[timezone %in% c("PST", "PDT")] <- "America/Los_Angeles"

  gmt_time <- vector(length = nrow(output))
  for(i in 1:nrow(output)) {
    gmt_time[i] <- as.character(lubridate::ymd_hms(output$datetime[i],
                                                   tz = timezone[i]), tz = "GMT")
  }
  #If any timezones are blank, don't output a GMT time
  gmt_time[output$TZ == ""] <- NA
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

  START_DATE <- START_TIME <- datetime <- ".dplyr.var"

  output <- dplyr::mutate(cyan_data, datetime = paste(START_DATE, START_TIME))

  #Sometimes 3 letter time zone abbreviations cause issues
  timezone <- output$TZ
  state <- output$STATE_CODE
  timezone[timezone %in% c("EST", "EDT")] <- "America/New_York"
  timezone[timezone %in% c("CST", "CDT")] <- "America/Chicago"
  timezone[timezone == "MST" & state == "AZ"] <- "America/Phoenix"
  timezone[timezone %in% c("MST", "MDT")] <- "America/Denver"
  timezone[timezone %in% c("PST", "PDT")] <- "America/Los_Angeles"

  local_tz <- output$LOCAL_TZ

  hour <- vector(length = nrow(output))
  for(i in 1:nrow(cyan_data)) {
    hour[i] <- as.character(lubridate::ymd_hms(output$datetime[i],
                                               tz = timezone[i]),
                            tz = local_tz[i], format = "%H")
  }
  hour <- as.numeric(hour)
  solar_noon <- hour %in% 10:13
  ext_solar_noon <- hour %in% 9:14
  solar_noon[output$TZ == "" | output$LOCAL_TZ == ""] <- NA
  ext_solar_noon[output$TZ == "" | output$LOCAL_TZ == ""] <- NA

  output$solar_noon <- solar_noon
  output$ext_solar_noon <- ext_solar_noon

  output <- dplyr::select(output, -datetime)
  return(output)

}

#' Add columns for trophic status
#'
#' Add three columns for trophic status, where it is possible. Trophic
#' status can be calculated from secchi depth, chlorophyll-a, or
#' total phosphorus
#'
#' @param cyan_data a data frame from \code{get_cyan_data} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with three additional columns. TROPHIC_STATUS_INDEX
#'  giving the numerical trophic status index,
#'  TROPHIC_STATUS - indicating which
#' trophic category that index falls into, and the method used to
#' calculate the trophic stats. TROPHIC_STATUS_METHOD
#'
#' @importFrom magrittr %>%
#'
#' @export

add_trophic_status <- function(cyan_data) {

  #Add columns
  cyan_data$TROPHIC_STATUS_INDEX <- NA
  cyan_data$TROPHIC_STATUS_METHOD <- NA
  cyan_data$TROPHIC_STATUS <- NA

  #Calculate trophic status based on secchi depth
  cyan_data$TROPHIC_STATUS_INDEX[cyan_data$PARAMETER_ID == "P0002"] <-
    60 - (14.41 * log(cyan_data$RESULT_VALUE[cyan_data$PARAMETER_ID == "P0002"]))
  cyan_data$TROPHIC_STATUS_METHOD[cyan_data$PARAMETER_ID == "P0002"] <- "SD"

  #Calculate trophic status based on chlorophyll-a
  cyan_data$TROPHIC_STATUS_INDEX[cyan_data$PARAMETER_ID == "P0051"] <-
    30.6 + (9.81 * log(cyan_data$RESULT_VALUE[cyan_data$PARAMETER_ID == "P0051"]))
  cyan_data$TROPHIC_STATUS_METHOD[cyan_data$PARAMETER_ID == "P0051"] <- "CHL-A"

  #Calculate trophic status based on total phosphorus
  cyan_data$TROPHIC_STATUS_INDEX[cyan_data$PARAMETER_ID == "P0031"] <-
    4.15 + (14.42 * log(cyan_data$RESULT_VALUE[cyan_data$PARAMETER_ID == "P0031"] * 1000))
  cyan_data$TROPHIC_STATUS_METHOD[cyan_data$PARAMETER_ID == "P0031"] <- "TP"

  #Assign trophic status based on the index
  cyan_data$TROPHIC_STATUS[cyan_data$TROPHIC_STATUS_INDEX >= 0 &
                             cyan_data$TROPHIC_STATUS_INDEX < 20] <- "Ultra-oligotrophy"
  cyan_data$TROPHIC_STATUS[cyan_data$TROPHIC_STATUS_INDEX >= 20 &
                             cyan_data$TROPHIC_STATUS_INDEX < 40] <- "Oligotrophy"
  cyan_data$TROPHIC_STATUS[cyan_data$TROPHIC_STATUS_INDEX >= 40 &
                             cyan_data$TROPHIC_STATUS_INDEX < 50] <- "Mesotrophy"
  cyan_data$TROPHIC_STATUS[cyan_data$TROPHIC_STATUS_INDEX >= 50 &
                             cyan_data$TROPHIC_STATUS_INDEX < 70] <- "Eutrophy"
  cyan_data$TROPHIC_STATUS[cyan_data$TROPHIC_STATUS_INDEX >= 70] <- "Hypereutrophy"

  return(cyan_data)

}

#' Connect to the CyAN database
#'
#' Generate a connection to the CyAN database or another sqlite database using
#' the CyAN schema and parameter table
#'
#' @param path a character string giving the path to the database
#'
#' @return a SQLite database connection
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' @export

connect_cyan <- function(path) {

  if(!file.exists(path)) {
    stop("file path doesn't exist, or isn't available")
  }

  cyan_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  table_names <- DBI::dbListTables(cyan_connection)

  if(!(all(c("ACTIVITY", "LOCATION", "RESULT", "PARAMETER", "METHOD") %in% table_names))) {
    stop("not a valid CyAN database")
  }

  return(cyan_connection)

}

#' Generate an index of locations and parameters from the database
#'
#' Based on the CyAN database connection, generate an index of all unique combinations
#' of locations and the parameters that have been collected there
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan()}
#'
#' @return a data frame of all unique combinations of location and parameter
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#' #Generate the location index
#' location_index <- generate_location_index(db_connection)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

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
    dplyr::select(LOCATION_ID, PARAMETER_ID) %>%
    dplyr::distinct() %>%
    dplyr::left_join(location, by = "LOCATION_ID") %>%
    dplyr::select(LOCATION_NAME, LATITUDE, LONGITUDE, PARAMETER_ID) %>%
    dplyr::collect()

  return(location_index)

}

#' Generate a table of parameter identifiers and their short names
#'
#' @param cyan_connection a CyAN database connection from \code{connect_cyan()}
#'
#' @return a data frame of parameter identifiers and their associated
#' short names
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#' #Generate the parameter index
#' parameter_index <- generate_parameter_index(db_connection)
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
#' @param collect a logical indicating whether the query will be pulled into
#' a local tibble using dbplyr::collect. If you are planning on doing further
#' selection or filtering using dplyr, you may want not want to collect until
#' you're ready
#' @param north_latitude,south_latitude numerics indicating the northern most
#' and sothern most latitude that will be included in the query. Latitude
#' should be given as a positive number of degrees north of the equator.
#'
#' @param west_longitude,east_longitude numerics indicating the western most
#' and eastern most latitude that will be included in the query. Longitude
#' should be given as a negative number of decimal degrees west of the prime
#' meridian.
#' @param years numeric vector of years that will be included in the query
#' @param parameters a character vector of parameter names that will be
#' returned in the query
#' @param minimum_tier a number between 1.0 and 4.0 indicating the minimum
#' data tier that will be returned in the query
#' @param states a character vector of the postal codes for the states to be
#' included in the query. Usually, this would not be used if querying by
#' latitude and longitude.
#' @return if collect is FALSE, the query will be generated, but not collected.
#' See the documentation on \code{collect()} for details. Otherwise, if collect
#' is TRUE, the query will be pulled into memory and returned as a tibble.
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the chlorophyll and chlorophyll-a data (parameter id P0051 & P0054)
#' #or the state of Kansas in the year 2016
#' ks_chl_2016 <- get_cyan_data(db_connection,
#'                              years = 2016,
#'                              parameters = c("P0051", "P0054"),
#'                              states = "KS")
#'
#' #Get chlorophyll-a data within a latitude longitude bounding box. If you
#' #need to reference the parameter table to find parameter codes, you can use
#' #generate_parameter_index()
#' chla_2017 <- get_cyan_data(db_connection,
#'                            north_latitude = 37.818, south_latitude = 37.714,
#'                            west_longitude = -98.028, east_longitude = -97.735,
#'                            years = 2017:2018,
#'                            parameters = "P0051")
#'
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
    LOCAL_TZ <- STRFTIME <- SAMPLE_TYPE_DEFINITION <- YEAR <- WQP_ACTIVITY_ID <- ".dplyr.var"

  location <- dplyr::tbl(cyan_connection, "LOCATION") %>%
    dplyr::select(LOCATION_ID, LOCATION_TYPE, LATITUDE, LONGITUDE, DATUM, STATE_CODE, HUC,
                  LOCATION_NAME, LOCAL_TZ)

  activity <- dplyr::tbl(cyan_connection, "ACTIVITY") %>%
    dplyr::select(ACTIVITY_ID, WQP_ACTIVITY_ID, START_DATE, START_TIME, END_DATE, END_TIME, TZ,
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
      invalid_states <- states[!(states %in% datasets::state.abb)]
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
#' @param parameter_1,parameter_2 CyAN parameter codes for the two parameters
#' of interest
#' @param north_latitude,south_latitude numbers indicating the northern most
#' and sothern most latitude that will be included in the query. Latitude
#' should be given as a positive number of degrees north of the equator.
#' @param west_longitude,east_longitude nunbers indicating the western most
#' and eastern most latitude that will be included in the query. Longitude
#' should be given as a negative number of decimal degrees west of the prime
#' meridian.
#' @param years numeric vector of years that will be included in the query
#' @param states a character vector of the postal codes for the states to be
#' included in the query. Usually, this would not be used if querying by
#' latitude and longitude.
#'
#' @return if collect is FALSE, the query will be generated, but not collected.
#' See the documentation on \code{collect} for details. Otherwise, if collect
#' is TRUE, the query will be pulled into memory and returned as a tibble.
#' Returns a wide data frame, with a shared columns related to the activity,
#' and columns related to the analytical result suffixed with a ".1" for parameter
#' 1 and ".2" for parameter 2.
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get simaltaneous observations of secchi depth (P0002) and chlorophyll-a (P0051)
#' biv_secchi_chla <- get_bivariate(db_connection,
#'                                  parameter_1 = "P0002", parameter_2 = "P0051",
#'                                  north_latitude = 37.818, south_latitude = 37.714,
#'                                  west_longitude = -98.028, east_longitude = -97.735)
#'
#' @importFrom magrittr %>%
#'
#' @export

get_bivariate <- function(cyan_connection, parameter_1, parameter_2,
                          north_latitude = NULL, south_latitude = NULL,
                          west_longitude = NULL, east_longitude = NULL,
                          years = NULL, states = NULL) {

  PARAMETER_ID <- ".dplyr.var"

  all_data <- get_cyan_data(cyan_connection, north_latitude = north_latitude,
                            south_latitude = south_latitude,
                            east_longitude = east_longitude,
                            west_longitude = west_longitude,
                            years = years,
                            states = states,
                            parameters = c(parameter_1, parameter_2),
                            collect = TRUE)
  parameter_1_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_1)
  parameter_2_data <- all_data %>%
    dplyr::filter(PARAMETER_ID == parameter_2)

  join_by <- names(all_data)[!(names(all_data) %in%
                                 c("RESULT_ID","PARAMETER_ID","METHOD_ID",
                                   "RESULT_DEPTH","RESULT_DEPTH_UNIT","RESULT_DEPTH_REFERENCE",
                                   "QUALIFIER","RESULT_VALUE","DETECTION_LIMIT_VALUE",
                                   "DETECTION_LIMIT_UNIT","DETECTION_LIMIT_TYPE","CROSSWALK_ID",
                                   "PARAMETER_NAME","UNITS","WQP_METHOD_IDENTIFIER",
                                   "WQP_METHOD_CONTEXT","WQP_METHOD_NAME","WQP_METHOD_DESCRIPTION",
                                   "SAMPLE_TYPE_DEFINITION","WQP_COLLECTION_METHOD_ID",
                                   "WQP_COLLECTION_METHOD_CONTEXT",
                                   "WQP_COLLECTION_METHOD_NAME"))]

  plot_data <- dplyr::inner_join(parameter_1_data, parameter_2_data,
                                 by = join_by,
                                 suffix = c(".1", ".2"))

  return(plot_data)

}

#' Add a column of GMT time to a cyan data query
#'
#' Give a column of GMT time based on the activity start time and local time zone
#'
#' @param cyan_data a data frame from \code{get_cyan_data()} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with an additional character column for time in GMT
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the chlorophyll and chlorophyll-a data (parameter id P0051 & P0054)
#' #or the state of Kansas in the year 2016
#' ks_chl_2016 <- get_cyan_data(db_connection, collect = TRUE,
#'                              years = 2016,
#'                              parameters = c("P0051", "P0054"),
#'                              states = "KS")
#'
#' #Add the column for GMT time
#' ks_chl_2016_wGMT <- add_GMT_time(ks_chl_2016)
#'
#'
#' @export
#'

add_GMT_time <- function(cyan_data) {

  START_DATE <- START_TIME <- datetime <- ".dplyr.var"

  output <- cyan_data %>%
    dplyr::mutate(datetime = paste(START_DATE, START_TIME))

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
                                                   tz = timezone[i]), tz = "GMT",
                                format = "%Y-%m-%d %H:%M:%S %Z")
  }

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
#' @param cyan_data a data frame from \code{get_cyan_data()} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with two additional logical columns, solar_noon indicating
#' if the sample was taken during solar noon and ext_solar_noon indicating if
#' the sample was taken during extended solar noon.
#'
#' @examples
#'
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the chlorophyll and chlorophyll-a data (parameter id P0051 & P0054)
#' #or the state of Kansas in the year 2016
#' ks_chl_2016 <- get_cyan_data(db_connection, collect = TRUE,
#'                              years = 2016,
#'                              parameters = c("P0051", "P0054"),
#'                              states = "KS",)
#'
#' #Add logical columns for solar noon and extended solar noon
#' ks_chl_2016_wsolarnoon <- add_solar_noon(ks_chl_2016)
#'
#' @export

add_solar_noon <- function(cyan_data) {

  START_DATE <- START_TIME <- datetime <- ".dplyr.var"

  output <- dplyr::mutate(cyan_data, datetime = paste(START_DATE, START_TIME))

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
#' @examples
#'
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the chlorophyll and chlorophyll-a data (parameter id P0051 & P0054)
#' #or the state of Kansas in the year 2016
#' ks_chl_2016 <- get_cyan_data(db_connection, collect = TRUE,
#'                              years = 2016,
#'                              parameters = c("P0051", "P0054"),
#'                              states = "KS")
#'
#' #Add trophic status columns
#' ks_chl_2016_wtrophic <- add_trophic_status(ks_chl_2016)
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

#' Add WHO thresholds
#'
#' Add columns indicating the WHO category for Cyanobacteria, Microcystin-LR, or
#' Cholorophyll-a, if applicable
#'
#' @param cyan_data a data frame from \code{get_cyan_data} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with an additional column WHO_CATEGORY indicating whether
#' this result is classified as Low, Moderate, High, or Very High based on
#' WHO thresholds for the parameter
#'
#' @examples
#'
#' #' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the chlorophyll and chlorophyll-a data (parameter id P0051 & P0054)
#' #or the state of Kansas in the year 2016
#' ks_chl_2016 <- get_cyan_data(db_connection, collect = TRUE,
#'                              years = 2016,
#'                              parameters = c("P0051", "P0054"),
#'                              states = "KS")
#'
#' #Add any applicable WHO categories
#' ks_chl_2016_wWHO <- add_WHO_category(ks_chl_2016)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

add_WHO_category <- function(cyan_data) {

  category <- rep(c("Low", "Moderate", "High", "Very High"), 3)
  lower_bound <- c(0, 20000, 100000, 10000000,
                   0, 10, 20, 2000,
                   0, 10, 50, 5000)
  upper_bound <- c(20000, 100000, 10000000, Inf,
                   10, 20, 2000, Inf,
                   10, 50, 5000, Inf)
  parameter <- c(rep("P0023", 4), rep("P0063", 4), rep("P0051", 4))
  who_cat <- data.frame(category, lower_bound, upper_bound, parameter, stringsAsFactors = FALSE)

  parm <- cyan_data$PARAMETER_ID
  value <- cyan_data$RESULT_VALUE
  cat <- vector(mode = "numeric", length = length(parm))

  for(i in seq_along(parm)) {
    cat_i <- who_cat %>%
      dplyr::filter(parameter == parm[i], lower_bound <= value[i], upper_bound > value[i]) %>%
      dplyr::pull(category)
    if(length(cat_i) != 1)
      cat_i <- NA
    cat[i] <- cat_i
  }

  cyan_data$WHO_CATEGORY <- cat
  return(cyan_data)

}

#' Add EPA recreational threshold
#'
#' Add a column indicating whether the EPA recreational trehsold was exceeded
#'
#' @param cyan_data a data frame from \code{get_cyan_data()} with
#' \code{collect = TRUE}
#'
#' @return cyan_data with an additional column EPA_REC_EXCEEDED - a logical column
#' indicating whether the applicable EPA recreational treshold was exceeded
#'
#' @examples
#'
#' #Connect to the example database bundled with CyAN
#' path <- system.file("extdata", "example.db", package = "CyAN")
#' db_connection <- connect_cyan(path)
#'
#' #Get all of the data for microcystis and cylindrospermopsin
#' ks_toxins <- get_cyan_data(db_connection, collect = TRUE,
#'                              parameters = c("P0073", "P0074"),
#'                              states = "KS")
#'
#' #Add any applicable recreational for toxins
#' ks_toxins_wEPA <- add_EPA_recreational_threshold(ks_toxins)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

add_EPA_recreational_threshold <- function(cyan_data) {

  cyan_data$EPA_REC_EXCEEDED <- NA

  cyan_data$EPA_REC_EXCEEDED[cyan_data$PARAMETER_ID == "P0073" &
                             cyan_data$RESULT_VALUE >= 8] <- TRUE
  cyan_data$EPA_REC_EXCEEDED[cyan_data$PARAMETER_ID == "P0073" &
                             cyan_data$RESULT_VALUE < 8] <- FALSE

  cyan_data$EPA_REC_EXCEEDED[cyan_data$PARAMETER_ID %in% c("P0074", "P0077") &
                               cyan_data$RESULT_VALUE >= 15] <- TRUE
  cyan_data$EPA_REC_EXCEEDED[cyan_data$PARAMETER_ID %in% c("P0074", "P0077") &
                               cyan_data$RESULT_VALUE < 15] <- FALSE

  return(cyan_data)

}

context("database connection and querying")

test_that("database connection works when pointed to valid database file", {
  expect_error(connect_cyan("test_invalid.db"), "valid database")
  expect_error(connect_cyan("not_here.db"), "file path")
  expect_is(connect_cyan("test.db"), "SQLiteConnection")
})

test_that("invalid parameter inputs stop before collecting data", {
  cyan <- connect_cyan("test.db")
  expect_error(get_cyan_data(cyan, north_latitude = "not a number"), "should be a number")
  expect_warning(get_cyan_data(cyan, north_latitude = 13.0), "south of the contiguous")
  expect_error(get_cyan_data(cyan, south_latitude = factor("not a number"), "should be a number"))
  expect_warning(get_cyan_data(cyan, south_latitude = 53.0), "north of the contiguous")
  expect_error(get_cyan_data(cyan, west_longitude = "not a number"), "should be a number")
  expect_warning(get_cyan_data(cyan, west_longitude = -50.3), "east of the contiguous")
  expect_error(get_cyan_data(cyan, east_longitude = "not a number"), "should be a number")
  expect_warning(get_cyan_data(cyan, east_longitude = -150.0), "west of the contiguous")
  expect_error(get_cyan_data(cyan, years = as.Date(c("2017-01-01", "2018-01-1"))), "vector of integers")
  expect_error(get_cyan_data(cyan, parameters = c("G0001", "G0002")), "G0002")
  expect_error(get_cyan_data(cyan, minimum_tier = "4.0"), "should be a number")
  expect_error(get_cyan_data(cyan, minimum_tier = 0.5), "minimum tier number")
  expect_error(get_cyan_data(cyan, states = c("Kansas", "WT")), "not valid state")
})

test_that("filtering data by location gives the expected results", {
  cyan <- connect_cyan("test.db")
  west <- get_cyan_data(cyan, east_longitude = -109, collect = TRUE) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(west %in% c("SANDGBAY", "RFFLAKE")))
  expect_true(all(c("SANDGBAY", "RFFLAKE") %in% west))
  east <- get_cyan_data(cyan, west_longitude = -93, collect = TRUE) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(east %in% c("RAYSTOWN", "CHOCBAY")))
  expect_true(all(c("RAYSTOWN", "CHOCBAY") %in% east))
  north <- get_cyan_data(cyan, south_latitude = 39.5) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(north %in% c("RAYSTOWN", "RFFLAKE")))
  expect_true(all(c("RAYSTOWN", "RFFLAKE") %in% north))
  south <- get_cyan_data(cyan, north_latitude = 35.0) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(south %in% c("SANDGBAY", "CHOCBAY")))
  expect_true(all(c("SANDGBAY", "CHOCBAY") %in% south))
  bounding_box <- get_cyan_data(cyan,
                                north_latitude = 40, south_latitude = 37,
                                west_longitude = -102, east_longitude = -64.5) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(bounding_box == "SMLAKE"))
  state_select <- get_cyan_data(cyan, states = c("PA", "CA")) %>%
    dplyr::pull(LOCATION_ID)
  expect_true(all(c("RAYSTOWN", "SANDGBAY") %in% state_select))
  expect_true(all(state_select %in% c("RAYSTOWN", "SANDGBAY")))

})

test_that("filtering by parameter gives the expected results", {
  cyan <- connect_cyan("test.db")
  #A single parameter
  chl <- get_cyan_data(cyan, parameters = "P0051") %>%
    dplyr::pull(PARAMETER_ID)
  expect_true(all(chl == "P0051"))
  chl_sd <- get_cyan_data(cyan, parameters = c("P0002", "P0051")) %>%
    dplyr::pull(PARAMETER_ID)
  #Multiple parameters
  expect_true(all(chl_sd %in% c("P0002", "P0051")))
  expect_true(all(c("P0002", "P0051") %in% chl_sd))
})

test_that("filtering by time gives the expected results", {
  cyan <- connect_cyan("test.db")
  chl_2011_2013 <- get_cyan_data(cyan, years = 2011:2013, collect = TRUE) %>%
    dplyr::mutate(year = as.character(as.Date(START_DATE), format = "%Y")) %>%
    dplyr::pull(year) %>%
    as.numeric()
  expect_true(all(chl_2011_2013 <= 2013) & all(chl_2011_2013 >= 2011))
})

test_that("bivariate data pull gives the expected results", {
  cyan <- connect_cyan("test.db")
  chl_sd <- get_bivariate(cyan, parameter_1 = "P0051", parameter_2 = "P0002", collect = TRUE)
  expect_equal(nrow(chl_sd), 5)
  expect_true(all(chl_sd$PARAMETER_ID.1 == "P0051") & all(chl_sd$PARAMETER_ID.2 == "P0002"))
})



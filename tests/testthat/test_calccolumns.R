context("calculated columns ")

#Test for correct solar noon, GMT additions
test_that("solar noon column calculated correctly", {
  cyan <- connect_cyan("test.db")
  cyan_data <- get_cyan_data(cyan, collect = TRUE) %>%
    add_solar_noon() %>%
    dplyr::mutate(hour = as.numeric(substr(START_TIME, 1, 2)))
  solar <- filter(cyan_data, solar_noon == TRUE) %>%
    pull(hour)
  expect_true(all(solar >= 10) & all(solar <= 14))
  ex_solar <- filter(cyan_data, ext_solar_noon == TRUE) %>%
    pull(hour)
  expect_true(all(ex_solar >= 9) & all(ex_solar <= 15))
  non_solar <- filter(cyan_data, solar_noon == FALSE, ext_solar_noon == FALSE) %>%
    pull(hour)
  expect_true(all(non_solar < 9 | non_solar > 15))
})

test_that("GMT column calculated correctly", {
  cyan <- connect_cyan("test.db")
  cyan_data <- get_cyan_data(cyan, collect = TRUE) %>%
    add_GMT_time()
  equal_datetimes <- vector(length = nrow(cyan_data))
  for(i in 1:nrow(cyan_data)) {
    start_date <- lubridate::ymd_hms(paste(cyan_data$START_DATE[i], cyan_data$START_TIME[i]),
                                     tz = cyan_data$TZ[i])
    start_date_gmt <- lubridate::ymd_hms(cyan_data$TIME_GMT[i], tz = "GMT")
    equal_datetimes[i] <- start_date == start_date_gmt
  }
  expect_true(all(equal_datetimes))
})

#Test for correct trophic status calculations based on each parameter
test_that("trophic status calculated correctly", {
  cyan <- connect_cyan("test.db")
  cyan_data <- get_cyan_data(cyan, collect = TRUE) %>%
    add_trophic_status()
  tp_method <- cyan_data %>%
    filter(PARAMETER_ID == "P0031") %>%
    pull(TROPHIC_STATUS_METHOD)
  expect_true(all(tp_method == "TP"))
  sd_method <- cyan_data %>%
    filter(PARAMETER_ID == "P0002") %>%
    pull(TROPHIC_STATUS_METHOD)
  expect_true(all(sd_method == "SD"))
  chla_method <- cyan_data %>%
    filter(PARAMETER_ID == "P0051") %>%
    pull(TROPHIC_STATUS_METHOD)
  expect_true(all(chla_method == "CHL-A"))
  r1wqp <- cyan_data %>%
    filter(WQP_ACTIVITY_ID == "R1WQP") %>%
    pull(TROPHIC_STATUS)
  expect_true(all(r1wqp == "Ultra-oligotrophy"))
  c1wqp <- cyan_data %>%
    filter(WQP_ACTIVITY_ID == "C1WQP") %>%
    pull(TROPHIC_STATUS)
  expect_true(all(c1wqp == "Oligotrophy"))
  s1wqp <- cyan_data %>%
    filter(WQP_ACTIVITY_ID == "S1WQP") %>%
    pull(TROPHIC_STATUS)
  expect_true(all(s1wqp == "Mesotrophy"))
  rf1wqp <- cyan_data %>%
    filter(WQP_ACTIVITY_ID == "RF1WQP") %>%
    pull(TROPHIC_STATUS)
  expect_true(all(rf1wqp == "Eutrophy"))
  sm1wqp <- cyan_data %>%
    filter(WQP_ACTIVITY_ID == "SM1WQP") %>%
    pull(TROPHIC_STATUS)
  expect_true(all(sm1wqp == "Hypereutrophy"))
})

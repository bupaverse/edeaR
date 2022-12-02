
test_that("test idle_time attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients.rda")

  idle_log <- patients %>%
    idle_time(level = "log", units = "auto")
  idle_trace <- patients %>%
    idle_time(level = "trace", units = "auto")
  idle_case <- patients %>%
    idle_time(level = "case", units = "auto")
  idle_resource <- patients %>%
    idle_time(level = "resource", units = "auto")

  expect_equal(attr(idle_log, "units"), "secs")
  expect_equal(attr(idle_trace, "units"), "secs")
  expect_equal(attr(idle_case, "units"), "secs")
  expect_equal(attr(idle_resource, "units"), "secs")
})

test_that("test idle_time attr 'units' set when arg 'units' = 'auto' on grouped_eventlog", {

  load("./testdata/patients_grouped.rda")

  # Filter out George Doe to avoid NA warnings.
  patients_grouped <- patients_grouped %>%
    filter_case(cases = "George Doe", reverse = TRUE)

  idle_log <- patients_grouped %>%
    idle_time(level = "log", units = "auto")
  idle_trace <- patients_grouped %>%
    idle_time(level = "trace", units = "auto")
  idle_case <- patients_grouped %>%
    idle_time(level = "case", units = "auto")
  idle_resource <- patients_grouped %>%
    idle_time(level = "resource", units = "auto")

  expect_equal(attr(idle_log, "units"), "mins")
  expect_equal(attr(idle_trace, "units"), "mins")
  expect_equal(attr(idle_case, "units"), "mins")
  expect_equal(attr(idle_resource, "units"), "secs")
})

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

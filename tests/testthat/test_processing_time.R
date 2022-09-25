
test_that("test processing_time attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients.rda")

  processing_log <- patients %>%
    processing_time(level = "log", units = "auto")
  processing_trace <- patients %>%
    processing_time(level = "trace", units = "auto")
  processing_case <- patients %>%
    processing_time(level = "case", units = "auto")
  processing_activity <- patients %>%
    processing_time(level = "activity", units = "auto")
  processing_resource <- patients %>%
    processing_time(level = "resource", units = "auto")
  processing_resource_activity <- patients %>%
    processing_time(level = "resource-activity", units = "auto")

  expect_equal(attr(processing_log, "units"), "secs")
  expect_equal(attr(processing_trace, "units"), "secs")
  expect_equal(attr(processing_case, "units"), "secs")
  expect_equal(attr(processing_activity, "units"), "secs")
  expect_equal(attr(processing_resource, "units"), "secs")
  expect_equal(attr(processing_resource_activity, "units"), "secs")
})

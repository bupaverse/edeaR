
test_that("test throughput_time attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients.rda")

  throughput_log <- patients %>%
    throughput_time(level = "log", units = "auto")
  throughput_trace <- patients %>%
    throughput_time(level = "trace", units = "auto")
  throughput_case <- patients %>%
    throughput_time(level = "case", units = "auto")

  expect_equal(attr(throughput_log, "units"), "secs")
  expect_equal(attr(throughput_trace, "units"), "secs")
  expect_equal(attr(throughput_case, "units"), "secs")
})


#### eventlog ####

test_that("test throughput_time attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients.rda")

  throughput_log <- patients %>%
    throughput_time(level = "log", units = "auto")
  throughput_trace <- patients %>%
    throughput_time(level = "trace", units = "auto")
  throughput_case <- patients %>%
    throughput_time(level = "case", units = "auto")

  expect_false(attr(throughput_log, "units") == "auto")
  expect_false(attr(throughput_trace, "units") == "auto")
  expect_false(attr(throughput_case, "units") == "auto")
})

test_that("test throughput_time on eventlog with arg 'level' = 'log'", {

  load("./testdata/patients.rda")

  throughput_time <- patients %>%
      throughput_time(level = "log", units = "mins")

  expect_s3_class(throughput_time, "log_metric")
  expect_s3_class(throughput_time, "throughput_time")

  # Expect 1 row with descriptive metrics: min, q1, median, mean, q3, max, st_dev, iqr
  expect_equal(dim(throughput_time), c(1, 8))
  expect_equal(colnames(throughput_time), c("min", "q1", "median", "mean", "q3", "max", "st_dev", "iqr"))

  # Test means to ensure correct output
  expect_equal(as.numeric(throughput_time$mean), 1103.916667)
  expect_s3_class(throughput_time$mean, "difftime")
  expect_equal(attr(throughput_time$mean, "units"), "mins")
})

test_that("test throughput_time on eventlog with arg 'level' = 'trace'", {

  load("./testdata/patients.rda")

  throughput_time <- patients %>%
      throughput_time(level = "trace", units = "mins")

  expect_s3_class(throughput_time, "trace_metric")
  expect_s3_class(throughput_time, "throughput_time")

  # Expect 3 rows with descriptive metrics: trace, relative_frequency, min, q1, mean, median, q3, max, st_dev, iqr, total
  expect_equal(dim(throughput_time), c(n_traces(patients), 11))
  expect_equal(colnames(throughput_time), c("trace", "relative_frequency", "min", "q1", "mean", "median", "q3", "max", "st_dev", "iqr", "total"))

  # Test means to ensure correct output
  expect_equal(as.numeric(throughput_time$mean), c(1819.16667, 1492.58333, 0.0))
  expect_s3_class(throughput_time$mean, "difftime")
  expect_equal(attr(throughput_time$mean, "units"), "mins")
})

test_that("test throughput_time on eventlog with arg 'level' = 'case'", {

  load("./testdata/patients.rda")

  throughput_time <- patients %>%
      throughput_time(level = "case", units = "mins")

  expect_s3_class(throughput_time, "case_metric")
  expect_s3_class(throughput_time, "throughput_time")

  # Expect 3 rows with processing times (1 for each case)
  expect_equal(dim(throughput_time), c(n_cases(patients), 2))
  expect_equal(colnames(throughput_time), c(case_id(patients), "throughput_time"))

  # Test processing time to ensure correct output
  expect_equal(as.numeric(throughput_time$throughput_time), c(1819.16667, 1492.58333, 0.0))
  expect_s3_class(throughput_time$throughput_time, "difftime")
  expect_equal(attr(throughput_time$throughput_time, "units"), "mins")
})

test_that("test throughput_time on grouped_eventlog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_grouped.rda")

  processing_log <- patients_grouped %>%
      throughput_time(level = "log", units = "auto")
  processing_trace <- patients_grouped %>%
      throughput_time(level = "trace", units = "auto")
  processing_case <- patients_grouped %>%
      throughput_time(level = "case", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
})


#### activitylog ####

test_that("test throughput_time on activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act.rda")

  processing_log <- patients_act %>%
      throughput_time(level = "log", units = "auto")
  processing_trace <- patients_act %>%
      throughput_time(level = "trace", units = "auto")
  processing_case <- patients_act %>%
      throughput_time(level = "case", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
})

test_that("test throughput_time on grouped_activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act_grouped.rda")

  processing_log <- patients_act_grouped %>%
      throughput_time(level = "log", units = "auto")
  processing_trace <- patients_act_grouped %>%
      throughput_time(level = "trace", units = "auto")
  processing_case <- patients_act_grouped %>%
      throughput_time(level = "case", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
})
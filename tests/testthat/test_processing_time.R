
test_that("test processing_time on eventlog with arg 'level' = 'log'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
    processing_time(level = "log", units = "mins")

  expect_s3_class(processing_time, "log_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 1 row with descriptive metrics: min, q1, median, mean, q3, max, st_dev, iqr
  expect_equal(dim(processing_time), c(1, 8))
  expect_equal(colnames(processing_time), c("min", "q1", "median", "mean", "q3", "max", "st_dev", "iqr"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$mean), 71.538889)
})

test_that("test processing_time on eventlog with arg 'level' = 'trace'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "trace", units = "mins")

  expect_s3_class(processing_time, "trace_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 3 rows with descriptive metrics: trace, relative_frequency, min, q1, mean, median, q3, max, st_dev, iqr, total
  expect_equal(dim(processing_time), c(n_traces(patients), 11))
  expect_equal(colnames(processing_time), c("trace", "relative_frequency", "min", "q1", "mean", "median", "q3", "max", "st_dev", "iqr", "total"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$mean), c(123.433333, 91.183333, 0.0))
})

test_that("test processing_time on eventlog with arg 'level' = 'case'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "case", units = "mins")

  expect_s3_class(processing_time, "case_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 3 rows with processing time
  expect_equal(dim(processing_time), c(n_cases(patients), 2))
  expect_equal(colnames(processing_time), c(case_id(patients), "processing_time"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$processing_time), c(123.433333, 91.183333, 0.0))
})

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

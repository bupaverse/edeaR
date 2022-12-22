
#### eventlog ####

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
  expect_s3_class(processing_time$mean, "difftime")
  expect_equal(attr(processing_time$mean, "units"), "mins")
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
  expect_s3_class(processing_time$mean, "difftime")
  expect_equal(attr(processing_time$mean, "units"), "mins")
})

test_that("test processing_time on eventlog with arg 'level' = 'case'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "case", units = "mins")

  expect_s3_class(processing_time, "case_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 3 rows with processing times (1 for each case)
  expect_equal(dim(processing_time), c(n_cases(patients), 2))
  expect_equal(colnames(processing_time), c(case_id(patients), "processing_time"))

  # Test processing time to ensure correct output
  expect_equal(as.numeric(processing_time$processing_time), c(123.433333, 91.183333, 0.0))
  expect_s3_class(processing_time$processing_time, "difftime")
  expect_equal(attr(processing_time$processing_time, "units"), "mins")
})

test_that("test processing_time on eventlog with arg 'level' = 'activity'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "activity", units = "mins")

  expect_s3_class(processing_time, "activity_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 5 rows with processing times (1 for each activity), with descriptive metrics: activity, min, q1, mean, median, q3, max, st_dev, iqr, total, relative_frequency
  expect_equal(dim(processing_time), c(n_activities(patients), 11))
  expect_equal(colnames(processing_time), c(activity_id(patients), "min", "q1", "mean", "median", "q3", "max", "st_dev", "iqr", "total", "relative_frequency"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$mean), c(26.875, 35.70556, 0.0, 0.0, 0.0), tolerance = 0.00001)
  expect_s3_class(processing_time$mean, "difftime")
  expect_equal(attr(processing_time$mean, "units"), "mins")
})

test_that("test processing_time on eventlog with arg 'level' = 'resource'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "resource", units = "mins")

  expect_s3_class(processing_time, "resource_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 5 rows with processing times (1 for each resource), with descriptive metrics: activity, min, q1, mean, median, q3, max, st_dev, iqr, total, relative_frequency
  expect_equal(dim(processing_time), c(n_resources(patients), 11))
  expect_equal(colnames(processing_time), c(resource_id(patients), "min", "q1", "mean", "median", "q3", "max", "st_dev", "iqr", "total", "relative_frequency"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$mean), c(0.0, 27.71667, 46.34167, 14.43333, 24.35), tolerance = 0.00001)
  expect_s3_class(processing_time$mean, "difftime")
  expect_equal(attr(processing_time$mean, "units"), "mins")
})

test_that("test processing_time on eventlog with arg 'level' = 'resource-activity'", {

  load("./testdata/patients.rda")

  processing_time <- patients %>%
      processing_time(level = "resource-activity", units = "mins")

  expect_s3_class(processing_time, "resource_activity_metric")
  expect_s3_class(processing_time, "processing_time")

  # Expect 57 rows with processing times (1 for each present resource-activity combination), with descriptive metrics:
  # activity, min, q1, mean, median, q3, max, st_dev, iqr, total, relative_frequency
  expect_equal(dim(processing_time), c(7, 12))
  expect_equal(colnames(processing_time), c(activity_id(patients), resource_id(patients), "min", "q1", "mean", "median",
                                            "q3", "max", "st_dev", "iqr", "total", "relative_frequency"))

  # Test means to ensure correct output
  expect_equal(as.numeric(processing_time$mean), c(0.0, 0.0, 0.0, 27.71667, 46.34167, 14.43333, 24.35), tolerance = 0.00001)
  expect_s3_class(processing_time$mean, "difftime")
  expect_equal(attr(processing_time$mean, "units"), "mins")
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

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_activity, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
  expect_false(attr(processing_resource_activity, "units") == "auto")
})

test_that("test processing_time on grouped_eventlog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_grouped.rda")

  processing_log <- patients_grouped %>%
      processing_time(level = "log", units = "auto")
  processing_trace <- patients_grouped %>%
      processing_time(level = "trace", units = "auto")
  processing_case <- patients_grouped %>%
      processing_time(level = "case", units = "auto")
  processing_activity <- patients_grouped %>%
      processing_time(level = "activity", units = "auto")
  processing_resource <- patients_grouped %>%
      processing_time(level = "resource", units = "auto")
  processing_resource_activity <- patients_grouped %>%
      processing_time(level = "resource-activity", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_activity, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
  expect_false(attr(processing_resource_activity, "units") == "auto")
})


#### activitylog ####

test_that("test processing_time on activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act.rda")

  processing_log <- patients_act %>%
      processing_time(level = "log", units = "auto")
  processing_trace <- patients_act %>%
      processing_time(level = "trace", units = "auto")
  processing_case <- patients_act %>%
      processing_time(level = "case", units = "auto")
  processing_activity <- patients_act %>%
      processing_time(level = "activity", units = "auto")
  processing_resource <- patients_act %>%
      processing_time(level = "resource", units = "auto")
  processing_resource_activity <- patients_act %>%
      processing_time(level = "resource-activity", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_activity, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
  expect_false(attr(processing_resource_activity, "units") == "auto")
})

test_that("test processing_time on grouped_activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act_grouped.rda")

  processing_log <- patients_act_grouped %>%
      processing_time(level = "log", units = "auto")
  processing_trace <- patients_act_grouped %>%
      processing_time(level = "trace", units = "auto")
  processing_case <- patients_act_grouped %>%
      processing_time(level = "case", units = "auto")
  processing_activity <- patients_act_grouped %>%
      processing_time(level = "activity", units = "auto")
  processing_resource <- patients_act_grouped %>%
      processing_time(level = "resource", units = "auto")
  processing_resource_activity <- patients_act_grouped %>%
      processing_time(level = "resource-activity", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_activity, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
  expect_false(attr(processing_resource_activity, "units") == "auto")
})

#### eventlog ####

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

  expect_false(attr(idle_log, "units") == "auto")
  expect_false(attr(idle_trace, "units") == "auto")
  expect_false(attr(idle_case, "units") == "auto")
  expect_false(attr(idle_resource, "units") == "auto")
})

test_that("test idle_time on eventlog with arg 'level' = 'log'", {

  load("./testdata/patients.rda")

  idle_time <- patients %>%
      idle_time(level = "log", units = "mins")

  expect_s3_class(idle_time, "log_metric")
  expect_s3_class(idle_time, "idle_time")

  # Expect 1 row with descriptive metrics: min, q1, median, mean, q3, max, st_dev, iqr
  expect_equal(dim(idle_time), c(1, 8))
  expect_equal(colnames(idle_time), c("min", "q1", "median", "mean", "q3", "max", "st_dev", "iqr"))

  # Test means to ensure correct output
  expect_equal(as.numeric(idle_time$mean), 87.416667)
  expect_s3_class(idle_time$mean, "difftime")
  expect_equal(attr(idle_time$mean, "units"), "mins")
})

test_that("test idle_time on eventlog with arg 'level' = 'trace'", {

  load("./testdata/patients.rda")

  idle_time <- patients %>%
      idle_time(level = "trace", units = "mins")

  expect_s3_class(idle_time, "trace_metric")
  expect_s3_class(idle_time, "idle_time")

  # Expect 2 rows with descriptive metrics: trace, min, q1, mean, median, q3, max, st_dev, iqr, total
  # George Doe has only 1 activity, so no idle time
  expect_equal(dim(idle_time), c(n_traces(patients) - 1, 10))
  expect_equal(colnames(idle_time), c("trace", "min", "q1", "mean", "median", "q3", "max", "st_dev", "iqr", "total"))

  # Test means to ensure correct output
  expect_equal(as.numeric(idle_time$mean), c(108.5166667, 66.3166667))
  expect_s3_class(idle_time$mean, "difftime")
  expect_equal(attr(idle_time$mean, "units"), "mins")
})

test_that("test idle_time on eventlog with arg 'level' = 'case'", {

  load("./testdata/patients.rda")

  idle_time <- patients %>%
      idle_time(level = "case", units = "mins")

  expect_s3_class(idle_time, "case_metric")
  expect_s3_class(idle_time, "idle_time")

  # Expect 2 rows with idle times (1 for each case)
  # George Doe has only 1 activity, so no idle time
  expect_equal(dim(idle_time), c(n_cases(patients) - 1, 2))
  expect_equal(colnames(idle_time), c(case_id(patients), "idle_time"))

  # Test idle time to ensure correct output
  expect_equal(as.numeric(idle_time$idle_time), c(108.5166667, 66.3166667))
  expect_s3_class(idle_time$idle_time, "difftime")
  expect_equal(attr(idle_time$idle_time, "units"), "mins")
})

test_that("test idle_time on eventlog with arg 'level' = 'resource'", {

  load("./testdata/patients.rda")

  idle_time <- patients %>%
      idle_time(level = "resource", units = "mins")

  expect_s3_class(idle_time, "resource_metric")
  expect_s3_class(idle_time, "idle_time")

  # Expect 2 rows with idle times (1 for each resource that has start & complete, and more than 1 instance, so only 'Richard' & 'Danny' comply),
  expect_equal(dim(idle_time), c(2, 2))
  expect_equal(colnames(idle_time), c(resource_id(patients), "idle_time"))

  # Test idle to ensure correct output
  expect_equal(as.numeric(idle_time$idle_time), c(1487.767, 1467.083), tolerance = 0.00001)
  expect_s3_class(idle_time$idle_time, "difftime")
  expect_equal(attr(idle_time$idle_time, "units"), "mins")
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

  expect_false(attr(idle_log, "units") == "auto")
  expect_false(attr(idle_trace, "units") == "auto")
  expect_false(attr(idle_case, "units") == "auto")
  expect_false(attr(idle_resource, "units") == "auto")
})


#### activitylog ####

test_that("test idle_time on activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act.rda")

  processing_log <- patients_act %>%
      idle_time(level = "log", units = "auto")
  processing_trace <- patients_act %>%
      idle_time(level = "trace", units = "auto")
  processing_case <- patients_act %>%
      idle_time(level = "case", units = "auto")
  processing_resource <- patients_act %>%
      idle_time(level = "resource", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
})

test_that("test idle_time on grouped_activitylog for attr 'units' set when arg 'units' = 'auto'", {

  load("./testdata/patients_act_grouped.rda")

  # Filter out George Doe to avoid NA warnings.
  patients_act_grouped <- patients_act_grouped %>%
      filter_case(cases = "George Doe", reverse = TRUE)

  processing_log <- patients_act_grouped %>%
      idle_time(level = "log", units = "auto")
  processing_trace <- patients_act_grouped %>%
      idle_time(level = "trace", units = "auto")
  processing_case <- patients_act_grouped %>%
      idle_time(level = "case", units = "auto")
  processing_resource <- patients_act_grouped %>%
      idle_time(level = "resource", units = "auto")

  expect_false(attr(processing_log, "units") == "auto")
  expect_false(attr(processing_trace, "units") == "auto")
  expect_false(attr(processing_case, "units") == "auto")
  expect_false(attr(processing_resource, "units") == "auto")
})
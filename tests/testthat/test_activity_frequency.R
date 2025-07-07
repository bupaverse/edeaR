
test_that("test activity_frequency log", {
	load("./testdata/patients.rda")

	activity_frequency <- patients %>%
		activity_frequency(level = "log")

	expect_s3_class(activity_frequency, "log_metric")
	expect_s3_class(activity_frequency, "activity_frequency")

	# Expect 1 row with descriptive metrics: min, q1, median, mean, q3, max, st_dev, iqr
	expect_equal(dim(activity_frequency), c(1, 8))
	expect_equal(colnames(activity_frequency), c("min", "q1", "median", "mean", "q3", "max", "st_dev", "iqr"))

	# Test means to ensure correct output
	expect_equal(as.numeric(activity_frequency$mean), 4)
	expect_type(activity_frequency$mean, "double")
})


processing_time_log <- function(log, units,	work_schedule) {

	raw <- processing_time_case(log, units = units, work_schedule = work_schedule)

	time_units <- attr(raw, "units")

	raw %>%
		pull(processing_time) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	attr(output, "units") <- time_units
	output
}

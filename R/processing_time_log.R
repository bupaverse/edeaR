

processing_time_log <- function(eventlog,
								units,
								work_schedule) {

	raw <- processing_time_case(eventlog, units = units, work_schedule = work_schedule)

	raw %>%
		pull(processing_time) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	output
}

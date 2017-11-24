

processing_time_log <- function(eventlog,
								units) {

	raw <- processing_time_case(eventlog, units = units)

	raw %>%
		pull(processing_time) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	output
}


throughput_time_log <- function(log, units,	work_schedule) {

	throughput_time_case(log, units = units, work_schedule = work_schedule) -> raw

	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(raw, "units")

	raw %>%
		pull(throughput_time) %>%
		#as.double(units = units) %>%
		summary_statistics() -> output

	attr(output, "raw") <- raw
	attr(output, "units") <- time_units
	return(output)
}

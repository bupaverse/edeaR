idle_time_log <- function(log, units) {

	idle_times <- idle_time_case(log, units)

	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(idle_times, "units")

	idle_times %>%
		pull(idle_time) %>%
		summary_statistics() -> output

	attr(output, "raw") <- idle_times
	attr(output, "units") <- time_units
	return(output)
}

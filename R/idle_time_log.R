idle_time_log <- function(eventlog, units) {

	idle_times <- idle_time_case(eventlog, units)

	idle_times %>%
		pull(idle_time) %>%
		summary_statistics() -> output

	attr(output, "raw") <- idle_times
	return(output)

}

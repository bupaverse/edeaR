idle_time_log <- function(eventlog, units) {

	idle_times <- idle_time_case(eventlog, units)

	output <-summary_statistics(idle_times$idle_time)

	attr(output, "raw") <- idle_times
	return(output)

}

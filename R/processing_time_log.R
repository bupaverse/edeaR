

processing_time_log <- function(eventlog,
								units = "days") {
	stop_eventlog(eventlog)

	r <- processing_time_case(eventlog, units = units)

	s <- summary_statistics(r$processing_time)

	attr(s, "raw") <- r
	return(s)
}

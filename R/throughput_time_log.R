


throughput_time_log <- function(eventlog,
								 units) {
	stop_eventlog(eventlog)

	r <- throughput_time_case(eventlog, units = units)

	s <- summary_statistics(r$throughput_time %>% as.double(units = units))

	attr(s, "raw") <- r

	return(s)
}

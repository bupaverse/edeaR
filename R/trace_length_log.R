


trace_length_log <- function(eventlog) {

	stop_eventlog(eventlog)

	csum <- trace_length_case(eventlog)

	s <- summary_statistics(csum$trace_length)
	attr(s, "raw") <- csum
	return(s)

}

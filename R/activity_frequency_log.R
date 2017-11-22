activity_frequency_log <- function(eventlog) {
	activity_frequency_case(eventlog) -> r
	output <- summary_statistics(r$absolute)
	attr(output, "raw") <- r
	return(output)
}

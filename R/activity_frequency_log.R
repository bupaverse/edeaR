activity_frequency_log <- function(log) {
	activity_frequency_case(log) -> r
	output <- summary_statistics(r$absolute)
	attr(output, "raw") <- r
	return(output)
}

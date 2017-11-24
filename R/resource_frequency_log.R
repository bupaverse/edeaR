
resource_frequency_log <- function(eventlog) {

	freq <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize() %>%
		summarize(freq = n()) -> raw

	output <- raw %>%
		pull(freq) %>%
		summary_statistics()

	attr(output, "raw") <- raw

	return(output)
}

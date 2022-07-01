
resource_frequency_log <- function(log) {

	freq <- NULL

	log %>%
		group_by(!!resource_id_(log), !!activity_instance_id_(log)) %>%
		#summarize() %>%
		summarize(freq = n()) -> raw

	output <- raw %>%
		pull(freq) %>%
		summary_statistics()

	attr(output, "raw") <- raw

	return(output)
}

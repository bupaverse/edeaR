
resource_frequency_log <- function(log) {

	freq <- NULL

	log %>%
		distinct(!!resource_id_(log), !!activity_instance_id_(log)) %>%
		group_by(!!resource_id_(log)) %>%
		summarize(freq = n()) -> raw

	output <- raw %>%
		pull(freq) %>%
		summary_statistics()

	attr(output, "raw") <- raw

	return(output)
}

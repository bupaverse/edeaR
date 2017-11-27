resource_specialisation_log <- function(eventlog) {

	freq <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize() %>%
		summarize(freq = n()) -> raw

	raw %>%
		pull(freq) %>%
		summary_statistics() -> output

	attr(output, "raw") <- raw

	return(output)

}

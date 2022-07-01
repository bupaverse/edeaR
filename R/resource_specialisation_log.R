resource_specialisation_log <- function(log) {

	freq <- NULL

	log %>%
		group_by(!!resource_id_(log), !!activity_id_(log)) %>%
		#summarize() %>%
		summarize(freq = n()) -> raw

	raw %>%
		pull(freq) %>%
		summary_statistics() -> output

	attr(output, "raw") <- raw

	return(output)

}

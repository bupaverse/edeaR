


filter_endpoints_percentile <- function(eventlog,
										percentage,
										reverse) {
	first_activity <- NULL
	last_activity <- NULL
	rel_freq <- NULL
	cum_freq <- NULL


	c_sum <- eventlog %>%
		cases

	c_sum %>%
		count(first_activity, last_activity) %>%
		mutate(rel_freq = n/sum(n)) %>%
		mutate(cum_freq = lag(cumsum(rel_freq), default = 0)) %>%
		filter(cum_freq < percentage) -> selected_pairs

	case_selection <- c_sum %>%
		filter(first_activity %in% selected_pairs$first_activity, last_activity %in% selected_pairs$last_activity) %>%
		pull(.data[[case_id(eventlog)]])

	filter_case(eventlog, case_selection, reverse)
}

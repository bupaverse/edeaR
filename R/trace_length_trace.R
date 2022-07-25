
trace_length_trace <- function(log) {

	trace <- NULL
	relative_trace_frequency <- NULL
	absolute <- NULL
	relative_to_median <- NULL


	tra <- case_list(log)

	log %>%
		trace_length_case() %>%
		merge(tra) %>%
		group_by(trace, absolute) %>%
		summarize(relative_trace_frequency = n()) %>%
		ungroup() %>%
		mutate(relative_trace_frequency = relative_trace_frequency/sum(relative_trace_frequency)) %>%
		mutate(relative_to_median = absolute/median(absolute))
}


trace_coverage_case <- function(eventlog) {

	trace_id <- NULL
	absolute_trace_coverage <- NULL

	case_list(eventlog) %>%
		group_by(trace) %>%
		mutate(absolute_trace_coverage = n(),
			   relative_trace_coverage = absolute_trace_coverage/n_cases(eventlog)) %>%
		ungroup() %>%
		arrange(desc(absolute_trace_coverage)) %>%
		select(-trace_id)
}

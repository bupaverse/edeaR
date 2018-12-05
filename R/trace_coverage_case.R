
trace_coverage_case <- function(eventlog) {

	trace_id <- NULL
	absolute_trace_coverage <- NULL

	absolute <- NULL

	case_list(eventlog) %>%
		group_by(trace) %>%
		mutate(absolute = n(),
			   relative = absolute/n_cases(eventlog)) %>%
		ungroup() %>%
		select(-trace_id)
}

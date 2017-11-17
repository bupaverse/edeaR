
trace_coverage_case <- function(eventlog) {

	tr <- cases_light(eventlog) %>%
		group_by(trace) %>%
		mutate(absolute_trace_coverage = n(),
			   relative_trace_coverage = absolute_trace_coverage/n_cases(eventlog)) %>%
		ungroup() %>%
		arrange(desc(absolute_trace_coverage)) %>%
		select(-trace_id)
	return(tr)
}

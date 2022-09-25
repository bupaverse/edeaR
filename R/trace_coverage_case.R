
trace_coverage_case <- function(log) {

	trace_id <- NULL
	absolute_trace_coverage <- NULL

	absolute <- NULL

	bupaR::case_list(log) %>%
		group_by(trace) %>%
		mutate(absolute = n(),
			   relative = absolute/n_cases(log)) %>%
		ungroup() %>%
		select(-trace_id)
}

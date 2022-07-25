
trace_coverage_trace <- function(log) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	absolute <- NULL
	relative <- NULL

	log %>%
		bupaR::traces() %>%
		select(trace, absolute = absolute_frequency, relative = relative_frequency) %>%
		arrange(desc(absolute)) %>%
		mutate(cum_sum = cumsum(relative)) %>%
		arrange(trace)
}

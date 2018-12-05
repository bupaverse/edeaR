
trace_coverage_trace <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		traces %>%
		select(trace, absolute = absolute_frequency, relative = relative_frequency) %>%
		arrange(desc(absolute)) %>%
		mutate(cum_sum = cumsum(relative)) %>%
		arrange(trace)

}

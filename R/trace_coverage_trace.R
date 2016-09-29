
trace_coverage_trace <- function(eventlog) {

	tr <- traces(eventlog)

	tr <- tr %>%
		select(trace, absolute = absolute_frequency, relative = relative_frequency) %>%
		arrange(desc(absolute)) %>%
		mutate(cum_sum = cumsum(relative))

	return(tr)
}
